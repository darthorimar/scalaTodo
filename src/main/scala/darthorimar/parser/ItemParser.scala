package darthorimar.parser

import fastparse.all._
import darthorimar.ast._
import fastparse.{all, core}

class ItemParser(indent: Int) {

  import ParserCommon._

  private val expression = ExpressionParser.parser

  private val arg = P(variable)
  private val funcDefItem =
    P("%%def" ~ " ".rep(min = 1) ~ variable ~ " ".rep ~ variable.rep(sep=" "))

  private val funcDefBlock =
    P(funcDefItem ~ blockBody) map { case (name, args, body) =>
      DefItem(name, args, body)
    }

  private val definitions =
    P(funcDefBlock.rep(sep = "\n"))

  private val textEntry =
    P(CharsWhile(!specialChars.contains(_)).!).map(TextEntry)
  private val expressionEntry =
    P("%{" ~ expression ~ "}").map(ExpressionEntry)
  private val ifItem =
    P("%if" ~ " ".rep ~ expression)
  private val elseItem =
    P("%else")
  private val defItem =
    P("%def" ~ " ".rep(min = 1) ~ variable ~ " ".rep ~ expressionEntry.rep(sep = " ")) map { case (name, args) =>
      FuncDefItem(name, args.map(_.expr))
    }
  private val loopItem =
    P("%loop" ~ " ".rep(min = 1) ~ variable ~ " ".rep ~ "<-" ~ " ".rep ~ expression)

  private val itemValue = P((textEntry | expressionEntry).rep(min = 1))

  val deeper: P[Int] = P(" ".rep(indent + 1).!.map(_.length))

  private val blockBody: P[Seq[Item]] = "\n" ~ deeper.flatMap { i =>
    new ItemParser(i).item.rep(1, sep = ("\n" + " " * i).~/)
  }
  private val block: P[Item] = P(itemValue ~ blockBody).map { case (i, is) =>
    SimpleItem(i, is)
  }
  private val ifBlock: all.Parser[(Expression, Seq[Item])] = P(ifItem ~ blockBody)
  private val elseBlock: all.Parser[Seq[Item]] =
    P("\n" ~ " ".rep(indent).!.map(_.length)).flatMap { i =>
      elseItem ~ blockBody
    }

  private val ifElseBlock = ifBlock ~ elseBlock.? map { case (cond, ifItems, elseItems) =>
    IfItem(cond, ifItems, elseItems.toSeq.flatten)
  }

  private val forBlock = loopItem ~ blockBody map { case (loopVar, range, body) =>
    LoopItem(loopVar, range, body)
  }

  private val item: P[Item] =
    P(block | ifElseBlock | forBlock | itemValue.map(SimpleItem(_)) | defItem)

  private val template: P[Template] =
    P((definitions ~ "\n").? ~ item.rep(sep = "\n") ~ End).map { case (defs, items) =>
      Template(defs.toSeq.flatten, items)
    }

  val parser: all.P[Template] = template
}

object ItemParser {
  def parse(template: String): Either[String, Template] =
    new ItemParser(0).parser.parse(template.trimRight).toEither
}