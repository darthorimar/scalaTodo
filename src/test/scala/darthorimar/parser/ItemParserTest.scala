package darthorimar.parser

import darthorimar.ast._
import utest._

import scala.collection.mutable.ArrayBuffer

object ItemParserTest extends TestSuite {
  private def testItem(items: String, expected: Seq[Item]): Unit = {
    val parsed = ItemParser().parser.parse(items).toEither
    assert(parsed.isRight)
    assert(parsed.right.get == expected)
  }

  private def testIncorrectItems(items: String): Unit = {
    val parsed = ItemParser().parser.parse(items).toEither
    assert(parsed.isLeft)
  }

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
            List()))
      testItem(items, expected)
    }

    'expressionInItemsTest - {
      val items =
        """%{1+2}
          | %{3}
          |  %{1+2}""".stripMargin
      val expected =
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
                    List()))))))
      testItem(items, expected)
    }
    'ifItemTest - {
      val items =
        """%if true or false
          | 42
          |%else
          | 0""".stripMargin
      val expected =
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
                List()))))

      testItem(items, expected)
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
                List()))))


      testItem(items, expected)
    }
  }
}