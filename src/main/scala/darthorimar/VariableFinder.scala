package darthorimar

import darthorimar.ast._

object VariableFinder {
  private def getVariables(tree: AST): Seq[String] = tree match {
    case Template(items) => items.flatMap(getVariables)
    case SimpleItem(value, subItems) =>
      value.flatMap(getVariables) ++ subItems.flatMap(getVariables)
    case IfItem(expr, ifBody, elseBody) =>
      getVariables(expr) ++ ifBody.flatMap(getVariables) ++ elseBody.flatMap(getVariables)
    case ExpressionEntry(expr) => getVariables(expr)
    case TextEntry(_) => Seq.empty
    case BinOp(_, left, right) =>
      getVariables(left) ++ getVariables(right)
    case _@(Number(_) | Str(_) | BoolConst(_)) => Seq.empty
    case VarRef(name) => Seq(name)
  }
  def listVariables(template: Template): Seq[String] =
    getVariables(template).distinct
}
