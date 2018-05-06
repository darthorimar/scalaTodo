package darthorimar.todolist.render

import darthorimar.todolist.ast._


class ConsoleRender extends Render {
  private def handleItem(item: String, indent: Int) =
    item.replaceAllLiterally("\n", "\n" + " " * indent)
  override private[render] def display(astMarker: AST, content: Seq[String], indent: Int): String = astMarker match {
    case  Template(title, _, _) =>
      s"$title\n${content.head}"
    case SimpleItem(_, is) if is.isEmpty =>
      s"${" "* indent}${handleItem(content.head, indent)}\n"
    case _: SimpleItem =>
      s"${" " * indent}${handleItem(content.head, indent)}\n${content(1)}"
  }
}
