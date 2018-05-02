package darthorimar

import java.io.File

import darthorimar.Frontend
import utest._

object FrontendTest extends TestSuite {
  private def file(filename: String): File = {
    new File(getClass.getResource(s"../$filename").getPath)
  }

  private def testFrontend(templateFile: String, configFileOpt: Option[String], expected: String) = {
    val frontend = new Frontend(file(templateFile), configFileOpt.map(file))
    val rendered = frontend.render
    assert(rendered.isRight)
    assert(rendered.right.get == expected)
  }

  val tests = Tests {
    'simpleFrontedTest - {
      val (template, conf) = ("words.templ", None)
      val expected =
        """Learn new words:
          | verbs:
          |  go
          |  eat
          | nouns:
          |  cat
          |  kitty
          |""".stripMargin
      testFrontend(template, conf, expected)
    }

    'variablesTest - {
      val (template, conf) = ("expression.templ", Option("expression.conf"))
      val expected =
        """42
          |""".stripMargin
      testFrontend(template, conf, expected)
    }
  }
}
