package darhorimar

import java.io.File

import darhorimar.parser.{ConfigParser, TemplateParser}
import darhorimar.renderer.{RenderConfig, Renderer}
import darhorimar.VariableFinder._

import scala.io.Source
import scala.util._

class Frontend(templateFile: File, configFileOpt: Option[File]) {

  private def readFile(file: File): Either[String, String] =
    Try(Source.fromFile(file).mkString).toEither.left.map(_.getMessage)

  private def generateConfig: Either[String, RenderConfig] =
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
  private def validateConf(conf: RenderConfig, variables: Seq[String]) =
    if (variables.toSet subsetOf conf.variables.keys.toSet)
      Right(())
    else
      Left(s"Parameters declaration missing ${(variables.toSet -- conf.variables.keys).mkString(", ")}")


  def render: Either[String, String] =
    for {
      template <- readFile(templateFile)
      parsed <- TemplateParser.parse(template)
      variables = VariableFinder.listVariables(parsed)
      conf <- generateConfig
      _ <- validateConf(conf, variables)
      rendered <- new Renderer(conf).render(parsed)
    } yield rendered

}
