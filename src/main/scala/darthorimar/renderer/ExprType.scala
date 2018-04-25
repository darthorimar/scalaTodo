package darthorimar.renderer

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