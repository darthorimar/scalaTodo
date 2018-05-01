package parser

import fastparse.all._
import ast._
import fastparse.all

object ExpressionParser {
  private def buildTree(tree: (Expression, Seq[(String, Expression)])) = {
    val (base, ops) = tree
    ops.foldLeft(base) { case (left, (op, right)) =>
      BinOp(op, left, right)
    }
  }

  private val number: P[Expression] =
    P(CharIn('0' to '9').rep(1).!.map(x => Number(x.toInt)))
  private val string: P[Expression] =
    P("\"" ~ CharsWhile(_ != '"') ~ "\"").rep(1).!.map(Str)
  private val parens: P[Expression] = P("(" ~/ addSub ~ ")")
  private val atom: P[Expression] = P(number | string | parens)

  private val divMul: P[Expression] =
    P(atom ~ (CharIn("*/").! ~/ atom).rep).map(buildTree)
  private val addSub: P[Expression] =
    P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(buildTree)
  private val compOp: P[Expression] =
    P(addSub ~ (("<" | "<=" | ">" | ">=" | "=").! ~/ addSub).rep).map(buildTree)
  val expression: P[Expression] = P(compOp)
}