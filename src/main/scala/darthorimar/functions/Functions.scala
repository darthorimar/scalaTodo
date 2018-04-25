package darthorimar.functions

import java.time.format.DateTimeFormatter

import darthorimar.renderer.{DateType, ExprType, IntType, StrType}

object Functions {
  private val functions: Map[String, PartialFunction[List[ExprType], ExprType]] =
    Map(
      "date" -> {
        case DateType(d)::Nil => StrType(d.format(DateTimeFormatter.ISO_LOCAL_DATE))
      },
      "time" -> {
        case DateType(d)::Nil => StrType(d.format(DateTimeFormatter.ISO_LOCAL_TIME))
      },
      "dateTime" -> {
        case DateType(d)::Nil => StrType(d.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      },
      "seconds" -> {
        case DateType(d)::Nil => IntType(d.getSecond)
      },
      "minutes" -> {
        case DateType(d)::Nil => IntType(d.getMinute)
      },
      "hours" -> {
        case DateType(d)::Nil => IntType(d.getHour)
      },
      "day" -> {
        case DateType(d)::Nil => IntType(d.getDayOfMonth)
      },
      "month" -> {
        case DateType(d)::Nil => IntType(d.getMonthValue)
      },
      "monthName" -> {
        case DateType(d)::Nil => StrType(d.getMonth.name)
      },
      "year" -> {
        case DateType(d)::Nil => IntType(d.getYear)
      }
    )
  def functionNames: Seq[String] = functions.keys.toSeq

  def callFunction(name: String, args: Seq[ExprType]): Either[String, ExprType] = {
    functions.get(name) match {
      case Some(f) =>
        if (f.isDefinedAt(args.toList))
          Right(f(args.toList))
        else
          Left(s"Bad arguments for functions $name")
      case None =>
        Left(s"Function $name not found")
    }
  }
}
