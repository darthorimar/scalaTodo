package darthorimar.functions

import java.nio.charset.{Charset, StandardCharsets}
import java.time.format.DateTimeFormatter

import darthorimar.renderer.{DateType, ExprType, IntType, SeqType, StrType}

import scala.io.Codec
import scala.util.{Random, Try}
import scala.xml.XML

object Functions {
  private val functions: Map[String, PartialFunction[List[ExprType], Either[String, ExprType]]] =
    Map(
      "date" -> {
        case DateType(d)::Nil => Right(StrType(d.format(DateTimeFormatter.ISO_LOCAL_DATE)))
      },
      "time" -> {
        case DateType(d)::Nil => Right(StrType(d.format(DateTimeFormatter.ISO_LOCAL_TIME)))
      },
      "dateTime" -> {
        case DateType(d)::Nil => Right(StrType(d.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)))
      },
      "seconds" -> {
        case DateType(d)::Nil => Right(IntType(d.getSecond))
      },
      "minutes" -> {
        case DateType(d)::Nil => Right(IntType(d.getMinute))
      },
      "hours" -> {
        case DateType(d)::Nil => Right(IntType(d.getHour))
      },
      "day" -> {
        case DateType(d)::Nil => Right(IntType(d.getDayOfMonth))
      },
      "month" -> {
        case DateType(d)::Nil => Right(IntType(d.getMonthValue))
      },
      "monthName" -> {
        case DateType(d)::Nil => Right(StrType(d.getMonth.name))
      },
      "year" -> {
        case DateType(d)::Nil => Right(IntType(d.getYear))
      },
      "bash" -> {
        case Nil =>
          implicit val codec = new Codec(Charset.forName("windows-1251"))
          val qs =
            for {
              rss <- Try(scala.io.Source.fromURL("https://bash.im/rss/"))
                        .map(_.getLines().mkString)
              xml <- Try(XML.loadString(rss))
              quotes =
              (xml \\ "channel" \ "item" \ "description")
                .map(_.text)
                .map { q =>
                  q.replace("&lt;", "<")
                    .replace("&gt;", ">")
                    .replace("&quot;", "\"")
                    .replace("<br>", "\n")
                }
            } yield quotes

          qs.toEither.left.map(_.toString)
            .map { q =>
              StrType(q(Random.nextInt(q.length)))
            }
      },
      "range" -> {
        case IntType(a)::IntType(b)::Nil =>
          Right(SeqType(a.until(b).map(IntType)))
        case IntType(b)::Nil =>
          Right(SeqType(0.until(b).map(IntType)))
      }
    )
  def functionNames: Seq[String] = functions.keys.toSeq

  def callFunction(name: String, args: Seq[ExprType]): Either[String, ExprType] = {
    functions.get(name) match {
      case Some(f) =>
        if (f.isDefinedAt(args.toList))
          f(args.toList)
        else
          Left(s"Bad arguments for functions $name")
      case None =>
        Left(s"Function $name not found")
    }
  }
}
