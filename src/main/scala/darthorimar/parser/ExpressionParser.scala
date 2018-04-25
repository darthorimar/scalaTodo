package darthorimar.parser

import java.time.LocalDateTime

import fastparse.all._
import darthorimar.ast._
import darthorimar.parser.ParserCommon.date
import darthorimar.renderer.DateType

object ExpressionParser {
  import White._
  import ParserCommon._

  private def buildTree(tree: (Expression, Seq[(String, Expression)])) = {
    val (base, ops) = tree
    ops.foldLeft(base) { case (left, (op, right)) =>
      BinOp(op, left, right)
    }
  }

  private val parens: P[Expression] = P("(" ~ baseExpr ~ ")")
  private val atom: P[Expression] =
    P(date.map(Date) |
      variable.map(VarRef) |
      number.map(Number) |
      string.map(Str) |
      boolConst.map(BoolConst) |
      parens)

  private val divMul: P[Expression] =
    P(atom ~ (CharIn("*/").! ~ atom).rep).map(buildTree)
  private val addSub: P[Expression] =
    P(divMul ~ (CharIn("+-").! ~ divMul).rep).map(buildTree)
  private val compOp: P[Expression] =
    P(addSub ~ (("<" | "<=" | ">" | ">=" | "=" | "!=").! ~ addSub).rep).map(buildTree)
  private val and: P[Expression] =
    P(compOp ~ ("and".! ~ compOp).rep).map(buildTree)
  private val xor: P[Expression] =
    P(and ~ ("or".! ~ and).rep).map(buildTree)
  private val or: P[Expression] =
    P(xor ~ ("xor".! ~ xor).rep).map(buildTree)

  private val baseExpr: P[Expression] = P(or)

  val parser: P[Expression] = P(baseExpr)
}