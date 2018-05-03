package darthorimar.todolist.render

import darthorimar.todolist._
import darthorimar.todolist.ast._

import scala.language.postfixOps

class WebRender extends Render {
  override def display(astMarker: AST, content: String, indent: Int): String = astMarker match {
    case _: Template =>
      s"""<div class="list-group list-group-root well">
         |$content
         |</div>""".stripMargin

    case SimpleItem(_, is) if is.isEmpty =>
      s"""<a href="#" class="list-group-item">
         |$content
         |</a>""".stripMargin

    case _: SimpleItem => content
  }
}



