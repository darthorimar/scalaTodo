package darthorimar.parser

import darthorimar.ast._

object TemplateParser {
  def parse(template: String): Either[String, Template] =
    ItemParser().parser.parse(template.trimRight).toEither.map(Template)
}
