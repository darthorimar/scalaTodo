package darthorimar.renderer

import darthorimar.ast._
import darthorimar.parser.ItemParser

class Renderer(conf: RenderConfig) {
  import Renderer._

  def render(template: Template): Result = {
    renderAst(template)(State(Map.empty, Map.empty))
  }

  private def evalExpr(expr: Expression)(implicit localVars: Map[String, ExprType]): ExprType = expr match {
    case Number(x) => IntType(x)
    case BoolConst(x) => BoolType(x)
    case Str(x) => StrType(x)
    case VarRef(x) =>
      (conf.variables ++ localVars).get(x) match {
        case Some(v) => v
        case None    => ErrorType(s"Variable $x not found")
      }
    case BinOp(op, left, right) =>
      val leftExpr = evalExpr(left)
      val rightExpr = evalExpr(right)
      (leftExpr, op, rightExpr) match {
        case (IntType(l), "+", IntType(r))  => IntType(l + r)
        case (IntType(l), "-", IntType(r))  => IntType(l - r)
        case (IntType(l), "*", IntType(r))  => IntType(l * r)
        case (IntType(l), "/", IntType(r))  => IntType(l / r)
        case (IntType(l), ">", IntType(r))  => BoolType(l > r)
        case (IntType(l), ">=", IntType(r)) => BoolType(l >= r)
        case (IntType(l), "<", IntType(r))  => BoolType(l < r)
        case (IntType(l), "<=", IntType(r)) => BoolType(l <= r)
        case (IntType(l), "=", IntType(r))  => BoolType(l == r)
        case (IntType(l), "!=", IntType(r)) => BoolType(l != r)

        case (BoolType(l), "and", BoolType(r)) => BoolType(l && r)
        case (BoolType(l), "or", BoolType(r))  => BoolType(l || r)
        case (BoolType(l), "xor", BoolType(r)) => BoolType(l ^ r)
        case (BoolType(l), "=", BoolType(r))   => BoolType(l == r)
        case (BoolType(l), "!=", BoolType(r))  => BoolType(l != r)

        case (StrType(l), "+", StrType(r))  => StrType(l + r)
        case (StrType(l), "=", StrType(r))  => BoolType(l == r)
        case (StrType(l), "!=", StrType(r)) => BoolType(l != r)

        case (ErrorType(msg1), _, ErrorType(msg2)) => ErrorType(s"$msg1, $msg2")
        case (_, _, ErrorType(msg)) => ErrorType(msg)
        case (ErrorType(msg), _, _) => ErrorType(msg)

        case _ => ErrorType(s"Can not evaluate expression ${leftExpr.show} $op ${rightExpr.show}")
      }
  }

  def renderItems(items: Seq[Item], indent: Int)(implicit state: State): Result =
    items.map(renderAst(_, indent)).sequence.map(_.mkString)

  private def renderAst(tree: AST, indent: Int = 0)(implicit state: State): Result = tree match {
    case Template(defs, items) =>
      val defsMap = defs.map(d => d.name -> d).toMap
      renderItems(items, 0)(State(defsMap, Map.empty))
    case SimpleItem(v, is) if is.isEmpty =>
      v.map(renderAst(_)).sequence.map(_.mkString)
        .map(x => s" " * indent + s"$x\n")

    case SimpleItem(v, is) =>
      for {
        x <- v.map(renderAst(_)).sequence.map(_.mkString)
        y <- renderItems(is, indent + 1)
      } yield " " * indent + s"$x\n" + y

    case TextEntry(text) => Right(text)
    case ExpressionEntry(e: Expression) =>
      evalExpr(e)(state.localVars) match {
        case ErrorType(message) => Left(message)
        case x => Right(x.show)
      }
    case IfItem(expr, ifBody, elseBody) =>
      evalExpr(expr)(state.localVars) match {
        case BoolType(cond) =>
          if (cond) renderItems(ifBody, indent)
          else renderItems(elseBody, indent)
        case ErrorType(message) => Left(message)
        case x => Left(s"${x.typeName} can not be used as condition")
      }
    case FuncDefItem(name, args) =>
      state.defs.get(name) match {
        case Some(d: FuncDef) =>
          if (args.length == d.args.length) {
            val locals = d.args.zip(args.map(evalExpr(_)(state.localVars))).toMap
            renderItems(d.body, indent)(state.copy(localVars = state.localVars ++ locals))
          } else Left(s"Wrong number of arguments for definition $name")
        case None =>
          Left(s"Definition $name not found")
      }
  }
}
object Renderer {
  type Result = Either[String, String]
  case class State(defs: Map[String, Def], localVars: Map[String, ExprType])

  def apply(conf: RenderConfig): Renderer = new Renderer(conf)
}
