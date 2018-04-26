package darthorimar.parser

import darthorimar.ast._
import utest._

import scala.collection.mutable.ArrayBuffer

object ItemParserTest extends TestSuite with ParserTestMixin {

  val tests = Tests {
    'plainItemsTest - {
      val items =
        """a
          | b
          | c
          | d e
          |  f g
          | h
          |i""".stripMargin
      val expected =
        Template(List(),
          ArrayBuffer(
            SimpleItem(
              ArrayBuffer(
                TextEntry("a")),
              ArrayBuffer(
                SimpleItem(
                  ArrayBuffer(
                    TextEntry("b")),
                  List()),
                SimpleItem(
                  ArrayBuffer(
                    TextEntry("c")),
                  List()),
                SimpleItem(
                  ArrayBuffer(
                    TextEntry("d e")),
                  ArrayBuffer(
                    SimpleItem(
                      ArrayBuffer(
                        TextEntry("f g")),
                      List()))),
                SimpleItem(
                  ArrayBuffer(
                    TextEntry("h")),
                  List()))),
            SimpleItem(
              ArrayBuffer(
                TextEntry("i")),
              List())))
      testParser(items, new ItemParser(0).parser, Some(expected))
    }
    'expressionInItemsTest - {
      val items =
        """%{1+2}
          | %{3}
          |  %{1+2}""".stripMargin
      val expected =
        Template(List(),
          ArrayBuffer(
            SimpleItem(
              ArrayBuffer(
                ExpressionEntry(
                  BinOp("+",
                    Number(1),
                    Number(2)))),
              ArrayBuffer(
                SimpleItem(
                  ArrayBuffer(
                    ExpressionEntry(
                      Number(3))),
                  ArrayBuffer(
                    SimpleItem(
                      ArrayBuffer(
                        ExpressionEntry(
                          BinOp("+",
                            Number(1),
                            Number(2)))),
                      List())))))))
      testParser(items, new ItemParser(0).parser, Some(expected))
    }
    'ifItemTest - {
      val items =
        """%if true or false
          | 42
          |%else
          | 0""".stripMargin
      val expected =
        Template(List(),
          ArrayBuffer(
            IfItem(
              BinOp("or",
                BoolConst(true),
                BoolConst(false)),
              ArrayBuffer(
                SimpleItem(
                  ArrayBuffer(
                    TextEntry("42")),
                  List())),
              List(
                SimpleItem(
                  ArrayBuffer(
                    TextEntry("0")),
                  List())))))

      testParser(items, new ItemParser(0).parser, Some(expected))
    }
    'nestedIfItemTest - {
      val items =
        """%if true
          | %if true
          |  %if false
          |   1
          |  %else
          |   4
          |%else
          | 0""".stripMargin
      val expected =
        Template(List(),
          ArrayBuffer(
            IfItem(
              BoolConst(true),
              ArrayBuffer(
                IfItem(
                  BoolConst(true),
                  ArrayBuffer(
                    IfItem(
                      BoolConst(false),
                      ArrayBuffer(
                        SimpleItem(
                          ArrayBuffer(
                            TextEntry("1")),
                          List())),
                      List(
                        SimpleItem(
                          ArrayBuffer(
                            TextEntry("4")),
                          List())))),
                  List())),
              List(
                SimpleItem(
                  ArrayBuffer(
                    TextEntry("0")),
                  List())))))

      testParser(items, new ItemParser(0).parser, Some(expected))
    }
    'defDeclarationTest - {
      val items =
        """%%def d a b
          | nya
          |%def d %{1} %{2}""".stripMargin
      val expected =
        Template(
          List(
            DefItem("d",
              ArrayBuffer("a", "b"),
              ArrayBuffer(
                SimpleItem(
                  ArrayBuffer(
                    TextEntry("nya")),
                  List())))),
          ArrayBuffer(
            FuncDefItem("d",
              ArrayBuffer(
                Number(1),
                Number(2)))))

      testParser(items, new ItemParser(0).parser, Some(expected))
    }

    'loopTest - {
      val items =
        """%loop i <- range(10)
          | nya""".stripMargin
      val expected =
        Template(
          List(),
          ArrayBuffer(
            LoopItem(
              "i",
              FuncCall(
                "range",
                ArrayBuffer(Number(10))),
              ArrayBuffer(
                SimpleItem(
                  ArrayBuffer(
                    TextEntry("nya")),
                  List())))))


      testParser(items, new ItemParser(0).parser, Some(expected))
    }
  }
}