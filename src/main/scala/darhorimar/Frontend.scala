package darhorimar

import java.io.File

import darhorimar.parser.{ConfigParser, TemplateParser}
import darhorimar.renderer.{ErrorType, ExprType, RenderConfig, Renderer}
import darhorimar.VariableFinder._

import scala.io.Source
import scala.util._

class Frontend(templateFile: File, configFileOpt: Option[File]) {

  private def readFile(file: File): Either[String, String] =
    Try(Source.fromFile(file).mkString).toEither.left.map(_.getMessage)

  private def generateConfig(variables: Seq[String]): Either[String, RenderConfig] = {
    val confE =
      configFileOpt match {
        case Some(configFile) =>
          for {
            conf <- readFile(configFile)
            parsed <- ConfigParser.parse(conf)
            _ = println(parsed)
          } yield parsed
        case None =>
          Right(RenderConfig(Map.empty))
      }
    confE match {
      case Right(conf) =>
        val notDefined = variables.toSet -- conf.variables.keys
        Right(interactiveConfig(conf,notDefined.toSeq))
      case Left(x) => Left(x)
    }
  }

  private def interactiveConfig(oldConf: RenderConfig, vars: Seq[String]) = {
    if (vars.isEmpty) oldConf
    else {
      println(s"To continue please define following variables: ${vars.mkString(", ")}")
      val newVars = collection.mutable.HashMap[String, ExprType]()
      for (v <-vars) {
        print(s"$v=")
        val x = scala.io.StdIn.readLine()
        ConfigParser.parseValue(x) match {
          case Right(x) =>
            newVars += v -> x
          case Left(_) => ()
        }
      }
      RenderConfig(oldConf.variables ++ newVars)
    }
  }

  def render: Either[String, String] =
    for {
      template <- readFile(templateFile)
      parsed <- TemplateParser.parse(template)
      variables = VariableFinder.listVariables(parsed)
      conf <- generateConfig(variables)
      rendered <- new Renderer(conf).render(parsed)
    } yield rendered
}

object Frontend {
  def apply(templateFile: File, configFileOpt: Option[File]): Frontend =
    new Frontend(templateFile, configFileOpt)
}