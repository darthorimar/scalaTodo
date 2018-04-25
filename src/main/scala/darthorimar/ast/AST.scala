package darthorimar.ast

import java.time.LocalDateTime

sealed trait AST

case class Template(defs: Seq[Def], items: Seq[Item]) extends AST

sealed trait ItemEntry extends AST
case class ExpressionEntry(expr: Expression) extends ItemEntry
sealed trait Item extends AST
case class TextEntry(text: String) extends ItemEntry
case class SimpleItem(value: Seq[ItemEntry], subItems: Seq[Item] = Seq.empty)
  extends Item

case class IfItem(expr: Expression, ifBody: Seq[Item], elseBody: Seq[Item]) extends Item
case class FuncDefItem(name: String, args: Seq[Expression]) extends Item

sealed trait Expression extends AST
case class BinOp(op: String, left: Expression, right: Expression) extends Expression
case class Number(num: Int) extends Expression
case class Str(value: String) extends Expression
case class BoolConst(value: Boolean) extends Expression
case class Date(value: LocalDateTime) extends Expression
case class VarRef(name: String) extends Expression

sealed trait Def extends Item {
  def name: String
}
case class FuncDef(name: String, args: Seq[String], body: Seq[Item]) extends Def
