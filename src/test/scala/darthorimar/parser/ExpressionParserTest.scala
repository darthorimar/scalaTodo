package darthorimar.parser

import darthorimar.ast._
import darthorimar.parser._
import utest._

object ExpressionParserTest extends TestSuite {
  private def testExpression(expr: String, expected: Expression): Unit = {
    val parsed = ExpressionParser.parser.parse(expr).toEither
    assert(parsed.isRight)
    assert(parsed.right.get == expected)
  }

  private def testIncorrectExpression(expr: String): Unit = {
    val parsed = ExpressionParser.parser.parse(expr).toEither
    assert(parsed.isLeft)
  }

  val tests = Tests {
    'ariphmicExpressionTest - {
      val expr = "1+2-3*(10*(2-3)/4)"
      val expected =
        BinOp("-",
          BinOp("+",
            Number(1),
            Number(2)),
          BinOp("*",
            Number(3),
            BinOp("/",
              BinOp("*",
                Number(10),
                BinOp("-",
                  Number(2),
                  Number(3))),
              Number(4))))
      testExpression(expr, expected)
    }

    'boolExpressionTest - {
      val expr = "true or x > 2 and y=a"
      val expected =
        BinOp("or",
          BoolConst(true),
          BinOp("and",
            BinOp(">",
              VarRef("x"),
              Number(2)),
            BinOp("=",
              VarRef("y"),
              VarRef("a"))))
      testExpression(expr, expected)
    }

    'stringExpressionTest - {
      val expr = """"a"+"b"="ab""""
      val expected =
        BinOp("=",
          BinOp("+",
            Str("a"),
            Str("b")),
          Str("ab"))
      testExpression(expr, expected)
    }

    'incorrectExpressionsTest - {
      testIncorrectExpression("1**2")
      testIncorrectExpression("1*(2-3")
      testIncorrectExpression("and or false")
    }
  }
}