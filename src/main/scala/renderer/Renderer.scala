package renderer

import ast._
import fastparse.core.Parsed
import parser.ItemParser

object Renderer {
  type Result = Either[String, String]

  sealed trait ExprResult{
    def show: String
    def typeName: String
  }
  case class IntResult(v: Int) extends ExprResult {
    override def show: String = v.toString
    override def typeName: String = "Int"
  }
  case class BoolResult(v: Boolean) extends ExprResult {
    override def show: String = v.toString
    override def typeName: String = "Bool"
  }
  case class StrResult(v: String) extends ExprResult {
    override def show: String = v
    override def typeName: String = "String"
  }
  case class ErrorResult(message: String) extends ExprResult {
      override def show: String = message
      override def typeName: String = "Error"
  }

  private def evalExpr(expr: Expression): ExprResult = expr match {
    case Number(x) => IntResult(x)
    case BoolConst(x) => BoolResult(x)
    case Str(x) => StrResult(x)
    case BinOp(op, left, right) =>
      val leftExpr = evalExpr(left)
      val rightExpr = evalExpr(right)
      (leftExpr, op, rightExpr) match {
        case (IntResult(l), "+", IntResult(r))  => IntResult(l + r)
        case (IntResult(l), "-", IntResult(r))  => IntResult(l - r)
        case (IntResult(l), "*", IntResult(r))  => IntResult(l * r)
        case (IntResult(l), "/", IntResult(r))  => IntResult(l / r)
        case (IntResult(l), ">", IntResult(r))  => BoolResult(l > r)
        case (IntResult(l), ">=", IntResult(r)) => BoolResult(l >= r)
        case (IntResult(l), "<", IntResult(r))  => BoolResult(l < r)
        case (IntResult(l), "<=", IntResult(r)) => BoolResult(l <= r)
        case (IntResult(l), "=", IntResult(r))  => BoolResult(l == r)
        case (IntResult(l), "!=", IntResult(r)) => BoolResult(l != r)

        case (BoolResult(l), "and", BoolResult(r)) => BoolResult(l && r)
        case (BoolResult(l), "or", BoolResult(r))  => BoolResult(l || r)
        case (BoolResult(l), "xor", BoolResult(r)) => BoolResult(l ^ r)
        case (BoolResult(l), "=", BoolResult(r))   => BoolResult(l == r)
        case (BoolResult(l), "!=", BoolResult(r))  => BoolResult(l != r)

        case (StrResult(l), "+", StrResult(r))  => StrResult(l + r)
        case (StrResult(l), "=", StrResult(r))  => BoolResult(l == r)
        case (StrResult(l), "!=", StrResult(r)) => BoolResult(l != r)

        case (ErrorResult(msg1), _, ErrorResult(msg2)) => ErrorResult(s"$msg1, $msg2")
        case (_, _, ErrorResult(msg)) => ErrorResult(msg)
        case (ErrorResult(msg), _, _) => ErrorResult(msg)

        case _ => ErrorResult(s"Can not evaluate expression ${leftExpr.show} $op ${rightExpr.show}")
      }
  }


  def renderItems(items: Seq[Item], indent: Int): Result =
    items.map(render(_, indent)).sequence.map(_.mkString)

  private def render(tree: AST, indent: Int = 0): Result = tree match {
    case SimpleItem(v, is) if is.isEmpty =>
      v.map(render(_)).sequence.map(_.mkString)
        .map(x => s" " * indent + s"$x\n")

    case SimpleItem(v, is) =>
      for {
        x <- v.map(render(_)).sequence.map(_.mkString)
        y <- renderItems(is, indent + 1)
      } yield " " * indent + s"$x:\n" + y

    case TextEntry(text) => Right(text)
    case ExpressionEntry(e: Expression) =>
      evalExpr(e) match {
        case ErrorResult(message) => Left(message)
        case x => Right(x.show)
      }
    case IfItem(expr, ifBody, elseBody) =>
      evalExpr(expr) match {
        case BoolResult(cond) =>
          if (cond) renderItems(ifBody, indent)
          else renderItems(elseBody, indent)
        case ErrorResult(message) => Left(message)
        case x => Left(s"${x.typeName} can not be used as if condition")
      }
  }
}
