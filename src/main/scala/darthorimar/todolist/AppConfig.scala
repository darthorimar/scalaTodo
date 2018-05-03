package darthorimar.todolist

import java.io.File

sealed trait DisplayMode {
  val name: String
}

object DisplayMode {
  def fromString(name: String): DisplayMode = name match {
    case ConsoleDisplayMode.name => ConsoleDisplayMode
  }
}

object ConsoleDisplayMode extends DisplayMode {
  override val name: String = "console"
}

case class AppConfig(templateFile: File,
                     confFileOpt: Option[File],
                     displayMode: DisplayMode)

object AppConfig {
  val empty: AppConfig = AppConfig(null, None, ConsoleDisplayMode)
}
