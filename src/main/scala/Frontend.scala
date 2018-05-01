import java.io.File

import parser.TempalteParser
import renderer.{RenderConfig, Renderer}

import scala.io.Source
import scala.util._

class Frontend(templateFile: File) {

  private def readFile(file: File): Either[String, String] =
    Try(Source.fromFile(file).mkString).toEither.left.map(_.getMessage)

  def render: Either[String, String] =
    for {
    template <- readFile(templateFile)
//    config <- readFile(configFile)
    parsed <- TempalteParser.parse(template)
    rendered <- new Renderer(RenderConfig(Map.empty)).render(parsed)
  } yield rendered

}
