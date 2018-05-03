package darthorimar.todolist.runner

import darthorimar.todolist.ast.Template
import darthorimar.todolist.render.RenderConfig

trait Runner {
  def run(text: String): Unit
}
