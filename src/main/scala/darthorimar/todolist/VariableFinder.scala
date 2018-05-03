package darthorimar.todolist

import darthorimar.todolist.ast._

object VariableFinder {
  private def getVariables(tree: AST)(implicit ignore: Seq[String] = Seq.empty): Seq[String] = tree match {
    case Template(defs, items) => 
      defs.flatMap(getVariables) ++ items.flatMap(getVariables)
    case DefItem(_, args, body) =>
      body.flatMap(getVariables(_)(ignore ++ args))
    case SimpleItem(value, subItems) =>
      value.flatMap(getVariables )++ subItems.flatMap(getVariables)
    case IfItem(expr, ifBody, elseBody) =>
      getVariables(expr) ++ ifBody.flatMap(getVariables) ++ elseBody.flatMap(getVariables)
    case ExpressionEntry(expr) => getVariables(expr)
    case TextEntry(_) => Seq.empty
    case BinOp(_, left, right) =>
      getVariables(left) ++ getVariables(right)
    case _@(Number(_) | Str(_) | BoolConst(_) | Date(_)) => Seq.empty
    case VarRef(name) =>
      if (ignore.contains(name)) Seq.empty
      else Seq(name)
    case FuncDefItem(_, args) =>
      args.flatMap(getVariables)
    case FuncCall(_, args) =>
      args.flatMap(getVariables)
    case SeqVal(value) =>
      value.flatMap(getVariables)
    case LoopItem(loopVar, range, body) =>
      getVariables(range) ++ body.flatMap(getVariables(_)(loopVar +: ignore))
  }
  def listVariables(template: Template): Seq[String] =
    getVariables(template).distinct
}
