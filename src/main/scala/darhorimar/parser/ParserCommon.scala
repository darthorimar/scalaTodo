package darhorimar.parser

import darhorimar.ast.VarRef
import fastparse.all._

object ParserCommon {
  val number: P[Int] =
    P(CharIn('0' to '9').rep(1)).!.map(_.toInt)
  val boolConst: P[Boolean] =
    P("true" | "false").!.map(_.toBoolean)
  val string: P[String] =
    P("\"" ~ CharsWhile(_ != '"') ~ "\"").rep(1).!
  val variable: P[String] =
    P((CharPred(_.isLetter) ~ CharsWhile(_.isLetterOrDigit).rep).!
      .filter(!keywords.contains(_)))
}
