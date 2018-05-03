package darthorimar

import java.io.File

import darthorimar.parser.{ConfigParser, ItemParser}
import darthorimar.renderer.{ExprType, RenderConfig, Renderer}

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

  private def interactiveConfig(oldConf: RenderConfig, vars: Seq[String]): RenderConfig = {
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
      val newConfig = RenderConfig(oldConf.variables ++ newVars)
      if (newVars.size == vars.size)
        newConfig
      else
        interactiveConfig(newConfig, (vars.toSet -- newVars.keys).toSeq)
    }
  }

  def render: Either[String, String] =
    for {
      template <- readFile(templateFile)
      parsed <- ItemParser.parse(template)
      variables = VariableFinder.listVariables(parsed)
      conf <- generateConfig(variables)
      rendered <- Renderer(conf).render(parsed)
    } yield rendered
}

object Frontend {
  def apply(templateFile: File, configFileOpt: Option[File]): Frontend =
    new Frontend(templateFile, configFileOpt)
}
