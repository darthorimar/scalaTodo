package darhorimar.parser

import fastparse.all._
import darhorimar.ast._
import fastparse.all

class ItemParser(indent: Int) {

  import ExpressionParser.expression

  private val textEntry =
    P(CharsWhile(!specialChars.contains(_)).!).map(TextEntry)
  private val expressionEntry =
    P("%{" ~ expression ~ "}").map(ExpressionEntry)
  private val ifItem =
    P("%if" ~ " ".rep  ~ expression)
  private val elseItem =
    P("%else")
  private val itemValue = P((textEntry | expressionEntry).rep(min = 1))

  val deeper: P[Int] = P(" ".rep(indent + 1).!.map(_.length))

  private val blockBody: P[Seq[Item]] = "\n" ~ deeper.flatMap(i =>
    ItemParser(indent = i).item.rep(1, sep = ("\n" + " " * i).~/)
  )
  private val block: P[Item] = P(itemValue ~ blockBody).map { case (i, is) =>
    SimpleItem(i, is)
  }
  private val ifBlock: all.Parser[(Expression, Seq[Item])] = P(ifItem ~ blockBody)
  private val elseBlock: all.Parser[Seq[Item]] =
    P("\n" ~ " ".rep(indent).!.map(_.length)).flatMap(i =>
      elseItem ~ blockBody
    )

  private val ifElseBlock = ifBlock ~ (elseBlock).? map {case (cond, ifItems, elseItems) =>
    IfItem(cond, ifItems, elseItems.toSeq.flatten)
  }

  val item: P[Item] =
    P(block | ifElseBlock | itemValue.map(SimpleItem(_)) )

  val parser: P[Seq[Item]] = P(item.rep(sep = "\n") ~ End)
}

object ItemParser {
  def apply(indent: Int = 0): ItemParser = new ItemParser(indent)
}