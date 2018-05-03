package darthorimar.todolist

import java.io.File

object Application extends App {


  val argsParser = new scopt.OptionParser[AppConfig]("todo.jar") {
    head("Todo List")

    opt[File]('t', "template").required().valueName("<file>")
      .action((x, c) =>
        c.copy(templateFile = x)).text("Template File")

    opt[String]('m', "mode").valueName("<web|console>")
      .action((x, c) =>
        c.copy(displayMode = DisplayMode.fromString(x))).text("Application mode")
      .validate(x =>
        if (x == ConsoleDisplayMode.name || x == WebDisplayMode.name) success
        else failure("Value mode must be either 'web' or 'console'")
      )

    opt[File]('c', "config").valueName("<file>")
      .action((x, c) =>
        c.copy(confFileOpt = Option(x))).text("Config file (optional)")

  }

  argsParser.parse(args, AppConfig.empty) match {
    case Some(conf) =>
      Frontend(conf).run()
    case None =>
  }
}
