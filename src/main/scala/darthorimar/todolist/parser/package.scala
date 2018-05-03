package darthorimar.todolist

import fastparse.WhitespaceApi
import fastparse.core.Parsed
import fastparse.core.Parsed.TracedFailure

package object parser {
  val specialChars = Seq('%', '\n', '}', '{')
  val keywords = Seq("true", "false", "and", "or", "xor")
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }

  implicit class ParsedOps[T, Elem, Repr](parsed: Parsed[T, Elem, Repr]) {
    def toEither: Either[String, T] = {
      def pos(failure: TracedFailure[Elem, Repr]) = {
        val i =failure.index
        val input = failure.input.slice(0, i).toString
        val lines = input.count(_ == '\n') + 1
        s"line $lines symbol ${input.split('\n').last.length}"
      }
      parsed.fold({ case (_, _ , info) =>
        Left(s"Syntax error on ${pos(info.traced)}")
      }, { case (v, _) =>
        Right(v)
      })
    }
  }

  implicit class StringOps(str: String) {
    def trimRight: String =
      str.replaceAll("\\s+$", "")
  }
}
