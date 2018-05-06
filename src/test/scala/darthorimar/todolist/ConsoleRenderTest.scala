package darthorimar.todolist

import darthorimar.todolist.parser.ItemParser
import darthorimar.todolist.render.{ConsoleRender, RenderConfig}
import utest._

object ConsoleRenderTest extends TestSuite {
  private def test(templateContent: String,
                   expected: String,
                   variables: Map[String, ExprType] = Map.empty): Unit = {
    val actual =
      for {
        template <- ItemParser.parse(templateContent)
        rendered <- new ConsoleRender().render(template, RenderConfig(variables))
      } yield rendered
    assert(actual.isRight)
    assert(actual.right.get.trim == expected.trim)
  }

  val tests = Tests {
    'basicConsoleRenderTest - {
      val templateContent =
        s"""#alpha todo
           |a:
           | b
           | c
           |  d
           | e""".stripMargin
      val expected =
        s"""alpha todo
           |a:
           | b
           | c
           |  d
           | e""".stripMargin
      test(templateContent, expected)
    }

    'expressionsConsoleRenderTest - {
      val templateContent =
        s"""%{1+2}
           |%{"a"+"b"}
           |%{range(4)}
           |%{true and false}
           |%{1<2}""".stripMargin
      val expected =
        s"""3
           |ab
           |(0, 1, 2, 3, 4)
           |false
           |true""".stripMargin
      test(templateContent, expected)
    }

    'ifConsoleRenderTest - {
      val templateContent =
        s"""%if 1<2
           | nya
           |%else
           | 42
           |%if true
           | 4
         """.stripMargin
      val expected =
        s"""nya
           |4""".stripMargin
      test(templateContent, expected)
    }

    'loopConsoleRenderTest - {
      val templateContent =
        s"""%loop i <- range(2)
           | %{i}""".stripMargin
      val expected =
        s"""0
           |1
           |2""".stripMargin
      test(templateContent, expected)
    }

    'defConsoleRenderTest - {
      val templateContent =
        s"""%%def megadef a b
           | %{a+b}
           |%def megadef %{1} %{2}""".stripMargin
      val expected =
        s"""3""".stripMargin
      test(templateContent, expected)
    }

    'varsConsoleRenderTest - {
      val templateContent =
        s"""%{x+y-z}""".stripMargin
      val expected =
        s"""11""".stripMargin
      val variables = Map("x" -> IntType(3), "y" -> IntType(10), "z" -> IntType(2))
      test(templateContent, expected, variables)
    }
  }
}
