package darthorimar.todolist.render

import darthorimar.todolist._
import darthorimar.todolist.ast._

import scala.language.postfixOps

class WebRender extends Render {
  override def display(astMarker: AST, content: Seq[String], indent: Int): String = astMarker match {
    case _: Template =>
      s"""<div class="list-group list-group-root well">
         |${content.head}
         |</div>""".stripMargin

    case SimpleItem(_, is) if is.isEmpty =>
      s"""<a href="#" class="list-group-item">
         |<input type="checkbox" aria-label="...">
         |${content.head}
         |</a>""".stripMargin

    case _: SimpleItem =>
      s"""<a href="#" class="list-group-item">
         |  <input type="checkbox" aria-label="...">
         |  ${content.head}
         |</a>
         |<div class="list-group">
         |${content(1)}
         |</div>""".stripMargin
  }
}



