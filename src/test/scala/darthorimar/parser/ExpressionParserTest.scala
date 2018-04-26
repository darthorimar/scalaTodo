package darthorimar.parser

import darthorimar.ast._
import darthorimar.parser._
import utest._

import fastparse.all._

object ExpressionParserTest extends TestSuite with ParserTestMixin {
  import ExpressionParser._

  val tests = Tests {
    'funcCallTest - {
      testParser("foo(1,2)", parser,
        Some(FuncCall("foo", Seq(Number(1), Number(2)))))
      testParser("foo()", parser,
        Some(FuncCall("foo", Seq.empty)))
      testParser("Foo()", parser, None)
    }

    'seqTest - {
      testParser("Seq(1,2)", parser,
        Some(SeqVal(Seq(Number(1), Number(2)))))
      testParser("Seq()", parser,
        Some(SeqVal(Seq.empty)))
    }

    'ariphmicExpressionTest - {
      val expr = "1+2-3*(10*(2-3)/4)"
      val expected: Expression =
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
      testParser(expr, parser, Some(expected))
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
      testParser(expr, parser, Some(expected))
    }

    'stringExpressionTest - {
      val expr = """"a"+"b"="ab""""
      val expected =
        BinOp("=",
          BinOp("+",
            Str("a"),
            Str("b")),
          Str("ab"))
      testParser(expr, parser, Some(expected))
    }

    'incorrectExpressionsTest - {
      testParser("1**2", parser, None)
      testParser("1*(2-3", parser, None)
      testParser("and or false", parser, None)
    }
  }
}