package darhorimar

import java.io.File

object Application extends App {

  case class CmdArgs(template: File = null, conf: Option[File] = None)

  val argsParser = new scopt.OptionParser[CmdArgs]("todo.jar") {
    head("Todo List")

    opt[File]('t', "template").required().valueName("<file>")
      .action((x, c) =>
        c.copy(template = x)).text("Template File")

    opt[File]('c', "config").valueName("<file>")
      .action((x, c) =>
        c.copy(conf = Option(x))).text("Config file (optional)")
  }

  argsParser.parse(args, CmdArgs()) match {
    case Some(conf) =>
      Frontend(conf.template, conf.conf).render match {
        case Right(todo) => println(todo)
        case Left(err) => println(s"Error: $err")
      }
    case None =>
  }
}
