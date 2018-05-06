package darthorimar.todolist.render

import darthorimar.todolist._
import darthorimar.todolist.ast._

import scala.language.postfixOps

class WebRender extends Render {
  private def escapeText(text: String) =
    text.replaceAllLiterally("<", "&lt;")
      .replaceAllLiterally(">", "&gt;")
      .replaceAllLiterally("\n", "<br>")

  private def renderItem(content: String) =
    s"""<div class="list-group-item">
       |<input type="checkbox" aria-label="...">
       |${escapeText(content)}
       |</div>""".stripMargin

  override private[render] def display(astMarker: AST, content: Seq[String], indent: Int): String =
    astMarker match {
      case Template(title, _, _) =>
        s"""<h1 class="text-center">$title</h1>
           |<div class="list-group list-group-root well">
           |${content.head}
           |</div>""".stripMargin

      case SimpleItem(_, is) if is.isEmpty =>
        renderItem(content.head)

      case _: SimpleItem =>
        s"""${renderItem(content.head)}
           |<div class="list-group">
           |${content(1)}
           |</div>""".stripMargin
    }
}



