package parser

import ast._

object TempalteParser {
  def parse(template: String): Either[String, Template] = {
    ItemParser().parser.parse(template)
      .fold({ case (_, _, info) =>
        Left(s"Syntax error ${info.traced.toString}")
      }, { case (items, _) =>
        Right(Template(items))
      })
  }
}
