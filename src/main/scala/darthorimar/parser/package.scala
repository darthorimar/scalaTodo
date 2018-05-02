package darthorimar

import darthorimar.ast.Template
import fastparse.WhitespaceApi
import fastparse.core.Parsed

package object parser {
  val specialChars = Seq('%', '\n', '}', '{')
  val keywords = Seq("true", "false", "and", "or", "xor")
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }

  implicit class ParsedOps[T, Elem, Repr](parsed: Parsed[T, Elem, Repr]) {
    def toEither: Either[String, T] =
      parsed.fold({ case (_, _, info) =>
        Left(
          s"""Syntax error
             |${info.traced.trace}""".stripMargin)
      }, { case (v, _) =>
        Right(v)
      })
  }

  implicit class StringOps(str: String) {
    def trimRight: String =
      str.replaceAll("^\\s+", "")
  }
}
