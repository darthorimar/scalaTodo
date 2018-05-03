package darthorimar.todolist.render

import darthorimar.todolist.ast._


class ConsoleRender extends Render {
  override def display(astMarker: AST, content: Seq[String], indent: Int): String = astMarker match {
    case _: Template =>
      content.head
    case SimpleItem(_, is) if is.isEmpty =>
      " " * indent + content.head + "\n"
    case _: SimpleItem =>
      " " * indent + content.mkString("\n")
  }
}
