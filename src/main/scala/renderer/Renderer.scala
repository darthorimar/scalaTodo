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
  def renderItems(items: Seq[Item], indent: Int): String =
    items.map(render(_, indent)).mkString
  def render(tree: AST, indent: Int = 0): String = tree match {
    case SimpleItem(v, is) if is.isEmpty =>
      " " * indent + s"${v.map(render(_)).mkString}\n"
    case SimpleItem(v, is) =>
      " " * indent + s"${v.map(render(_)).mkString}:\n" +
        renderItems(is, indent + 1)
    case TextEntry(text) => text
    case ExpressionEntry(e: IfExpr) => evalExpr(e).toString
    case ExpressionEntry(e: Expression) => evalExpr(e).toString
    case IfItem(expr, ifBody, elseBody) =>
      if(evalExpr(expr) != 0) renderItems(ifBody, indent)
      else renderItems(elseBody, indent)
  }

  def main(args: Array[String]): Unit = {
    val a =
      s"""%if(0)
         | e
         |%else
         | b
       """.stripMargin.trim
    val Parsed.Success(ast, x) = ItemParser().parser.parse(a).get
    println(ast)
    println("")
    println(renderItems(ast, indent = 0))
  }
}
