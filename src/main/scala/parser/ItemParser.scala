package parser

import fastparse.all._
import ast._

class ItemParser(indent: Int) {

  import ExpressionParser._

  private val textEntry =
    P(CharsWhile(!specialChars.contains(_)).!).map(TextEntry)
  private val expressionEntry =
    P("%{" ~ expression ~ "}").map(ExpressionEntry)
  private val itemEntries = P((textEntry | expressionEntry).rep(min = 1))

  val deeper: P[Int] = P(" ".rep(indent + 1).!.map(_.length))

  private val blockBody: P[Seq[Item]] = "\n" ~ deeper.flatMap(i =>
    ItemParser(indent = i).item.rep(1, sep = ("\n" + " " * i).~/)
  )
  private val block: P[Item] = P(itemEntries ~ blockBody).map { case (i, is) =>
    Item(i, is)
  }

  val item: P[Item] = P(block | itemEntries.map(Item(_)))

  val parser: P[Item] = P(block ~ End)
}

object ItemParser {
  def apply(indent: Int = 0): ItemParser = new ItemParser(indent)
}