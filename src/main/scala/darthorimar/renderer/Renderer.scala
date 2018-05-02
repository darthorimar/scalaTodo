package darthorimar.renderer

import darthorimar.ast._
import darthorimar.parser.ItemParser

class Renderer(conf: RenderConfig) {
  import Renderer._

  def render(template: Template): Result = {
    renderItems(template.items, 0)
  }

  private def evalExpr(expr: Expression): ExprType = expr match {
    case Number(x) => IntType(x)
    case BoolConst(x) => BoolType(x)
    case Str(x) => StrType(x)
    case VarRef(x) =>
      conf.variables.get(x) match {
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

  def renderItems(items: Seq[Item], indent: Int): Result =
    items.map(renderAst(_, indent)).sequence.map(_.mkString)

  private def renderAst(tree: AST, indent: Int = 0): Result = tree match {
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
      evalExpr(e) match {
        case ErrorType(message) => Left(message)
        case x => Right(x.show)
      }
    case IfItem(expr, ifBody, elseBody) =>
      evalExpr(expr) match {
        case BoolType(cond) =>
          if (cond) renderItems(ifBody, indent)
          else renderItems(elseBody, indent)
        case ErrorType(message) => Left(message)
        case x => Left(s"${x.typeName} can not be used as condition")
      }
  }
}
object Renderer {
  type Result = Either[String, String]
}
