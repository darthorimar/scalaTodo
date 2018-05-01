package parser

import fastparse.all._
import ast._

object ExpressionParser {
  import White._

  private def buildTree(tree: (Expression, Seq[(String, Expression)])) = {
    val (base, ops) = tree
    ops.foldLeft(base) { case (left, (op, right)) =>
      BinOp(op, left, right)
    }
  }

  private val number: P[Expression] =
    P(CharIn('0' to '9').rep(1).!.map(x => Number(x.toInt)))
  private val boolConst: P[Expression] =
    P("true" | "false").!.map(x => BoolConst(x.toBoolean))
  private val string: P[Expression] =
    P("\"" ~ CharsWhile(_ != '"') ~ "\"").rep(1).!.map(Str)
  private val variable: P[VarRef] =
    P((CharPred(_.isLetter) ~ CharsWhile(_.isLetterOrDigit).rep).!
      .filter(!keywords.contains(_))).map(VarRef)
  private val parens: P[Expression] = P("(" ~/ baseExpr ~ ")")
  private val atom: P[Expression] = P(variable | number | string | boolConst | parens)

  private val divMul: P[Expression] =
    P(atom ~ (CharIn("*/").! ~/ atom).rep).map(buildTree)
  private val addSub: P[Expression] =
    P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(buildTree)
  private val compOp: P[Expression] =
    P(addSub ~ (("<" | "<=" | ">" | ">=" | "=" | "!=").! ~/ addSub).rep).map(buildTree)
  private val and: P[Expression] =
    P(compOp ~ ("and".! ~/ compOp).rep).map(buildTree)
  private val xor: P[Expression] =
    P(and ~ ("or".! ~/ and).rep).map(buildTree)
  private val or: P[Expression] =
    P(xor ~ ("xor".! ~/ xor).rep).map(buildTree)

  private val baseExpr: P[Expression] = P(or)

  val expression: P[Expression] = P(baseExpr)
}