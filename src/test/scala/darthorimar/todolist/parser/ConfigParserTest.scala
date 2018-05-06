package darthorimar.todolist.parser

import darthorimar.todolist._
import darthorimar.todolist.parser.ConfigParser
import darthorimar.todolist.render.RenderConfig
import utest._

object ConfigParserTest extends TestSuite {
  private def test(configContent: String,
                   expected: RenderConfig): Unit = {
    val actual = ConfigParser.parse(configContent)
    assert(actual.isRight)
    assert(actual.right.get == expected)
  }

  val tests = Tests {
    'configParserTest - {
      val configContent =
        """a=5
          |b="b"
          |c=true
          |d=Seq(12,"qwe")""".stripMargin
      val expected = RenderConfig(Map(
        "a" -> IntType(5),
        "b" -> StrType("b"),
        "c" -> BoolType(true),
        "d" -> SeqType(Seq(IntType(12), StrType("qwe")))
      ))
      test(configContent, expected)
    }
  }
}
