package darthorimar.todolist.render

import darthorimar.todolist._
import darthorimar.todolist.ast._
import darthorimar.todolist.render.Render.{State, evalExpr}

trait Render {
  private[render] def display(astMarket: AST, content: Seq[String], indent: Int): String

  def render(template: Template, renderConf: RenderConfig): Result[String] =
    renderAst(template)(State.empty, renderConf)

  private def renderItems(items: Seq[Item], indent: Int)
                         (implicit state: State, renderConf: RenderConfig): Result[String] =
    items.map(renderAst(_, indent)).sequence.map(_.mkString)


  private def renderAst(tree: AST, indent: Int = 0)
                       (implicit state: State, renderConf: RenderConfig): Result[String] = tree match {
    case Template(title, defs, items) =>
      val defsMap = defs.map(d => d.name -> d).toMap
      renderItems(items, 0)(State(defsMap, Map.empty), renderConf)
        .map(x => display(tree, Seq(x), indent))
    case SimpleItem(v, is) if is.isEmpty =>
      v.map(renderAst(_)).sequence.map(_.mkString)
        .map(x => display(tree, Seq(x), indent))
    case SimpleItem(v, is) =>
      (for {
        x <- v.map(renderAst(_)).sequence.map(_.mkString)
        y <- renderItems(is, indent + 1)
      } yield Seq(x, y))
        .map(display(tree, _, indent))
    case TextEntry(text) => Right(text)
    case ExpressionEntry(e: Expression) =>
      evalExpr(e)(state.localVars, renderConf).map(_.show)
    case IfItem(expr, ifBody, elseBody) =>
      evalExpr(expr)(state.localVars, renderConf) match {
        case Right(BoolType(cond)) =>
          if (cond) renderItems(ifBody, indent)
          else renderItems(elseBody, indent)
        case Left(message) => Left(message)
        case Right(t) => Left(s"${t.typeName} can not be used as condition")
      }
    case FuncDefItem(name, args) =>
      state.defs.get(name) match {
        case Some(d: DefItem) =>
          if (args.length == d.args.length) {
            args.map(evalExpr(_)(state.localVars, renderConf)).sequence match {
              case Right(rs) =>
                val locals = d.args.zip(rs).toMap
                renderItems(d.body, indent)(state.copy(localVars = state.localVars ++ locals), renderConf)
              case Left(message) => Left(message)
            }
          } else Left(s"Wrong number of arguments for definition $name")
        case None =>
          Left(s"Definition $name not found")
      }
    case LoopItem(loopVar, range, body) =>
      evalExpr(range)(state.localVars, renderConf) match {
        case Right(SeqType(r)) =>
          val items =
            r map { i =>
              val newState = state.copy(localVars = state.localVars + (loopVar -> i))
              renderItems(body, indent)(newState, renderConf)
            } sequence

          items.map(_.mkString)
        case Right(_) => Left("Loop range shoulbd be Seq")
        case Left(message) => Left(message)
      }
  }
}

object Render {
  case class State(defs: Map[String, MetaItem], localVars: Map[String, ExprType])

  object State {
    def empty: State = State(Map.empty, Map.empty)
  }

  private[render] def evalExpr(expr: Expression)
                              (implicit localVars: Map[String, ExprType],
                               renderConf: RenderConfig): Result[ExprType] = expr match {
    case Number(x)    => Right(IntType(x))
    case BoolConst(x) => Right(BoolType(x))
    case Str(x)       => Right(StrType(x))
    case Date(x)      => Right(DateType(x))
    case SeqVal(x) =>
      x.map(evalExpr).sequence match {
        case Right(rs) =>
          Right(SeqType(rs))
        case Left(message) => Left(message)
      }
    case VarRef(x) =>
      localVars.get(x) match {
        case Some(v) => Right(v)
        case None =>
          renderConf.variables.get(x) match {
            case Some(v) => Right(v)
            case None    => Left(s"Variable $x not found")
          }
      }
    case FuncCall(name, args) =>
      args.map(evalExpr).sequence match {
        case Right(rs) =>
          Functions.callFunction(name, rs)
        case Left(message) => Left(message)
      }
    case BinOp(op, left, right) =>
      val leftExpr = evalExpr(left)
      val rightExpr = evalExpr(right)
      (leftExpr, op, rightExpr) match {
        case (Right(IntType(l)), "+", Right(IntType(r)))  => Right(IntType(l + r))
        case (Right(IntType(l)), "-", Right(IntType(r)))  => Right(IntType(l - r))
        case (Right(IntType(l)), "*", Right(IntType(r)))  => Right(IntType(l * r))
        case (Right(IntType(l)), "/", Right(IntType(r)))  => Right(IntType(l / r))
        case (Right(IntType(l)), ">", Right(IntType(r)))  => Right(BoolType(l > r))
        case (Right(IntType(l)), ">=", Right(IntType(r))) => Right(BoolType(l >= r))
        case (Right(IntType(l)), "<", Right(IntType(r)))  => Right(BoolType(l < r))
        case (Right(IntType(l)), "<=", Right(IntType(r))) => Right(BoolType(l <= r))
        case (Right(IntType(l)), "=", Right(IntType(r)))  => Right(BoolType(l == r))
        case (Right(IntType(l)), "!=", Right(IntType(r))) => Right(BoolType(l != r))

        case (Right(BoolType(l)), "and", Right(BoolType(r))) => Right(BoolType(l && r))
        case (Right(BoolType(l)), "or", Right(BoolType(r)))  => Right(BoolType(l || r))
        case (Right(BoolType(l)), "xor", Right(BoolType(r))) => Right(BoolType(l ^ r))
        case (Right(BoolType(l)), "=", Right(BoolType(r)))   => Right(BoolType(l == r))
        case (Right(BoolType(l)), "!=", Right(BoolType(r)))  => Right(BoolType(l != r))

        case (Right(StrType(l)), "+", Right(StrType(r)))  => Right(StrType(l + r))
        case (Right(StrType(l)), "=", Right(StrType(r)))  => Right(BoolType(l == r))
        case (Right(StrType(l)), "!=", Right(StrType(r))) => Right(BoolType(l != r))

        case (Right(DateType(l)), "=", Right(DateType(r))) => Right(BoolType(l == r))
        case (Right(DateType(l)), "!=", Right(DateType(r))) => Right(BoolType(l != r))

        case (Left(msg1), _, Left(msg2)) => Left(s"$msg1, $msg2")
        case (_, _, Left(msg)) => Left(msg)
        case (Left(msg), _, _) => Left(msg)

        case (Right(l), _, Right(r)) => Left(s"Can not evaluate expression ${l.show} $op ${r.show}")
      }
  }
}
