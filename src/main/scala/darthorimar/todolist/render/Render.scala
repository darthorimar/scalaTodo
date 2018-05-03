package darthorimar.todolist.render

import darthorimar.todolist.ast.Template

trait Render {
  def render(template: Template, renderConf: RenderConfig): Result[String]
}
