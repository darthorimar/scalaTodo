package darhorimar.parser

import darhorimar.renderer.{BoolType, ExprType, IntType, RenderConfig, StrType}
import fastparse.all._
import fastparse.core

object ConfigParser {
  import ParserCommon._
  private val value =
    P(number.map(IntType) |
      string.map(StrType) |
      boolConst.map(BoolType))
  private val entry =
    P(variable ~ "=" ~ value)
  private val parser: P[Map[String, ExprType]] =
    P(entry.rep(sep="\n")).map(Map(_:_*))

  def parse(conf: String): Either[String, RenderConfig] =
    parser.parse(conf).toEither.map(RenderConfig)
}
