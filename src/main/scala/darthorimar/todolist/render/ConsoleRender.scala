package darthorimar.todolist.render

import darthorimar.todolist.ast._


class ConsoleRender extends Render {
  override def display(astMarker: AST, content: String, indent: Int): String = astMarker match {
    case _: Template =>
      content
    case SimpleItem(_, is) if is.isEmpty =>
      " " * indent + content + "\n"
    case _: SimpleItem =>
      " " * indent + content
  }
}
