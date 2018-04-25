package darthorimar.parser

import java.sql.Timestamp
import java.time.{LocalDate, LocalDateTime, LocalTime}

import darthorimar.ast.VarRef
import fastparse.all._

import scala.util.Try

object ParserCommon {
  private def parseDateTime(dateTime: String): Try[LocalDateTime] = {
    Try(LocalDateTime.parse(dateTime))
      .recoverWith { case _ =>
        Try(LocalDate.parse(dateTime))
          .map(_.atStartOfDay)
      }
      .recoverWith { case _ =>
        Try(LocalTime.parse(dateTime))
          .map(_.atDate(LocalDate.MIN))
      }
  }
  val number: P[Int] =
    P(CharIn('0' to '9').rep(1)).!.map(_.toInt)
  val boolConst: P[Boolean] =
    P("true" | "false").!.map(_.toBoolean)
  val string: P[String] =
    P("\"" ~ CharsWhile(_ != '"').! ~ "\"")
  val variable: P[String] =
    P((CharPred(_.isLetter) ~ CharsWhile(_.isLetterOrDigit).rep).!
      .filter(!keywords.contains(_)))
  var date: P[LocalDateTime] =
    P("Date(" ~ CharsWhile(_ != ')').! ~ ")").filter { d =>
      parseDateTime(d).isSuccess
    } map {d =>
      parseDateTime(d).get
    }
}
