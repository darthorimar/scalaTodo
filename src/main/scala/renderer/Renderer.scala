package renderer

import ast._
import fastparse.core.Parsed
import parser.ItemParser

object Renderer {
  def evalExpr(expr: Expression): Int = expr match {
    case Number(x) => x
    case BinOp(op, left, right) =>
      op match {
        case "+" => evalExpr(left) + evalExpr(right)
        case "-" => evalExpr(left) - evalExpr(right)
        case "*" => evalExpr(left) * evalExpr(right)
        case "/" => evalExpr(left) / evalExpr(right)
      }
  }
  def render(tree: AST, indent: Int = 0): String = tree match {
    case Item(v, is) if is.isEmpty =>
      " " * indent + s"${v.map(render(_)).mkString}\n"
    case Item(v, is) =>
      " " * indent + s"${v.map(render(_)).mkString}:\n" +
        is.map(render(_, indent + 1)).mkString

    case TextEntry(text) => text
    case ExpressionEntry(e: Expression) => evalExpr(e).toString
  }

  def main(args: Array[String]): Unit = {
    val a =
      s"""fds%{1+1*2*3}gfd
        | b
        | c
        |  d
        | f
        |  r
      """.stripMargin.trim
    val Parsed.Success(ast, x) = ItemParser().parser.parse(a).get
    println(ast)
    println("")
    println(render(ast))
  }
}
