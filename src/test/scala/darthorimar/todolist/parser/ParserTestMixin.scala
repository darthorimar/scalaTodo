package darthorimar.todolist.parser

import fastparse.all._

trait ParserTestMixin {
  def testParser[A](text: String, paser: P[A], expected: Option[A]): Unit = {
    val res = (paser  ~ End).parse(text).toEither.toOption
    utest.assert(res.isDefined == expected.isDefined)
    if (res.isDefined)
      utest.assert(res.get == expected.get)
  }
}
