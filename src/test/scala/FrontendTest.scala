import java.io.File

import darhorimar.Frontend
import utest._

import scala.io.Source

object FrontendTest extends TestSuite {
  def file(filename: String): File = {
    new File(getClass.getResource(filename).getPath)
  }
  val tests = Tests {
    'simpleFrontedTest - {
      val frontend = new Frontend(file("simpleTest.templ"), None)
      println(frontend.render match {
        case Left(x)  => x
        case Right(x) => x
      })
    }
    'variablesFrontedTest - {
      val frontend =
        new Frontend(file("expression.templ"),
          Some(file("expression.conf")))
      println(frontend.render match {
        case Left(x)  => x
        case Right(x) => x
      })
    }
  }
}