import java.io.File

import utest._

import scala.io.Source

object FrontendTest extends TestSuite {
  def templateFile(filename: String): File = {
    new File(getClass.getResource(filename).getPath)
  }
  val tests = Tests {
    'simpleFrontedTest - {
      val frontend = new Frontend(templateFile("simpleTest.templ"))
      println(frontend.render match {
        case Left(x)  => x
        case Right(x) => x
      })
    }
  }
}