package darthorimar.todolist

import java.io.File

import darthorimar.todolist.parser.{ConfigParser, ItemParser}
import darthorimar.todolist.render.{ConsoleRender, Render, RenderConfig, WebRender}
import darthorimar.todolist.runner.{ConsoleRunner, Runner}

import scala.io.Source
import scala.util._

class Frontend(config: AppConfig) {
  import config._

  private def readFile(file: File): Either[String, String] =
    Try(Source.fromFile(file).mkString).toEither.left.map(_.getMessage)

  private def generateRenderConfig(variables: Seq[String]): Either[String, RenderConfig] = {
    val confE =
      confFileOpt match {
        case Some(confFile) =>
          for {
            conf <- readFile(confFile)
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
      for (varName <-vars) {
        print(s"$varName=")
        val varValue = scala.io.StdIn.readLine()
        ConfigParser.parseValue(varValue) match {
          case Right(x) => newVars += varName -> x
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

  private val runner: Runner = displayMode match {
    case ConsoleDisplayMode => new ConsoleRunner
    case WebDisplayMode     => new ConsoleRunner
  }

  private val render: Render = displayMode match {
    case ConsoleDisplayMode => new ConsoleRender
    case WebDisplayMode     => new WebRender
  }


  def run(): Unit = {
    (for {
      templateContent <- readFile(templateFile)
      template <- ItemParser.parse(templateContent)
      variables = VariableFinder.listVariables(template)
      renderConf <- generateRenderConfig(variables)
      rendered <- render.render(template, renderConf)
    } yield rendered) match {
      case Right(text) => runner.run(text)
      case Left(error) => println(error)
    }
  }
}

object Frontend {
  def apply(config: AppConfig): Frontend = new Frontend(config)
}
