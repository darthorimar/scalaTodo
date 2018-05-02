package darhorimar.parser

import darhorimar.ast._

object TemplateParser {
  def parse(template: String): Either[String, Template] =
    ItemParser().parser.parse(template.trimRight).toEither.map(Template)
}
