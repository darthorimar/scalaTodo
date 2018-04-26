package darthorimar.renderer

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

sealed trait ExprType {
  def show: String
  def typeName: String
}

case class IntType(v: Int) extends ExprType {
  override def show: String = v.toString
  override def typeName: String = "Int"
}

case class BoolType(v: Boolean) extends ExprType {
  override def show: String = v.toString
  override def typeName: String = "Bool"
}

case class StrType(v: String) extends ExprType {
  override def show: String = v
  override def typeName: String = "String"
}

case class DateType(v: LocalDateTime) extends ExprType {
  override def show: String = v.format(DateTimeFormatter.ISO_DATE_TIME)
  override def typeName: String = "Date"
}

case class SeqType(v: Seq[ExprType]) extends ExprType {
  override def show: String = s"(${v.map(_.show).mkString(", ")})"
  override def typeName: String = "Seq"
}