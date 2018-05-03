package darthorimar.todolist

import java.nio.charset.Charset
import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, TextStyle}
import java.util.Locale

import scala.io.Codec
import scala.util.{Random, Try}
import scala.xml.XML

object Functions {
  private def dateRange[A](from :LocalDateTime,
                        to: LocalDateTime,
                        next: LocalDateTime => LocalDateTime,
                        cmp: LocalDateTime => A) = {
    def generateRange(x: LocalDateTime, range: Seq[LocalDateTime]): Seq[LocalDateTime] = {
      if (cmp(x) == cmp(to)) x +: range
      else generateRange(x.plusDays(1), x +: range)
    }
    val range = generateRange(from, Seq.empty)
    SeqType(range.reverse.map(DateType))
  }
  private val functions: Map[String, PartialFunction[List[ExprType], Either[String, ExprType]]] =
    Map(
      "now" -> {
        case Nil => Right(DateType(LocalDateTime.now))
      },
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
      "dayOfWeek" -> {
        case DateType(d)::Nil => Right(StrType(d.getDayOfWeek.getDisplayName(TextStyle.FULL, Locale.ENGLISH)))
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
      "format" -> {
        case DateType(d)::StrType(f)::Nil => Right(StrType(d.format(DateTimeFormatter.ofPattern(f))))
      },
      "bash" -> {
        case Nil =>
          implicit val codec: Codec = new Codec(Charset.forName("windows-1251"))
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
          Right(SeqType(a.to(b).map(IntType)))
        case IntType(b)::Nil =>
          Right(SeqType(0.to(b).map(IntType)))
      }
      ,
      "dayRange" -> {
        case DateType(a)::DateType(b)::Nil =>
          Right(dateRange(a, b, _.plusDays(1), _.toLocalDate))
      },
      "monthRange" -> {
        case DateType(a)::DateType(b)::Nil =>
          Right(dateRange(a, b, _.plusMonths(1), _.toLocalDate))
      },
      "hourRange" -> {
        case DateType(a)::DateType(b)::Nil =>
          Right(dateRange(a, b, _.plusHours(1), identity))
      },
      "minuteRange" -> {
        case DateType(a)::DateType(b)::Nil =>
          Right(dateRange(a, b, _.plusMinutes(1), identity))
      },
      "rand" -> {
        case IntType(a)::IntType(b)::Nil =>
          Right(IntType(Random.nextInt(b - a + 1) + a))
      },
      "randChoice" -> {
        case SeqType(s)::Nil =>
          Right(s(Random.nextInt(s.length)))
      },
      "randSubseq" -> {
        case SeqType(s)::IntType(c)::Nil =>
          Right(SeqType(Random.shuffle(s).take(c)))
      },
      "addWeek" -> {
        case DateType(s)::Nil =>
          Right(DateType(s.plusWeeks(1)))
      },
      "contains" -> {
        case SeqType(s)::(e: ExprType)::Nil =>
          Right(BoolType(s.contains(e)))
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
