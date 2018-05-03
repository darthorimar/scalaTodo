package darthorimar.todolist.parser

import java.time.{LocalDate, LocalDateTime}

import darthorimar.todolist._
import darthorimar.todolist.render._
import fastparse.all._
import fastparse.core

object ConfigParser {
  import ParserCommon._

  private val seq: P[SeqType] =
    P("Seq(" ~ value.rep(sep= "," ~ " ".rep) ~ ")").map(SeqType)
  private val value =
    P(number.map(IntType) |
      string.map(StrType) |
      boolConst.map(BoolType) |
      date.map(DateType) |
      seq)

  private val entry =
    P(variable ~ "=" ~ value)
  private val parser: P[Map[String, ExprType]] =
    P(entry.rep(sep = "\n")).map(Map(_: _*))

  def parse(conf: String): Either[String, RenderConfig] =
    parser.parse(conf).toEither.map(RenderConfig)

  def parseValue(v: String): Either[String, ExprType] =
    value.parse(v).toEither
}
