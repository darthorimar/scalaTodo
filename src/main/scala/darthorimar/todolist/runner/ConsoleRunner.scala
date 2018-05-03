package darthorimar.todolist.runner

class ConsoleRunner extends Runner {
  override def run(text: String): Unit = {
    println(text)
  }
}
