package ast

sealed trait AST

sealed trait ItemEntry extends AST
case class ExpressionEntry(expr: Expression) extends ItemEntry
case class TextEntry(text: String) extends ItemEntry
case class Item(value: Seq[ItemEntry], subItems: Seq[Item] = Seq.empty) extends AST

sealed trait Expression
case class BinOp(op: String, left: Expression, right: Expression) extends Expression
case class Number(num: Int) extends Expression
case class Str(value: String) extends Expression
