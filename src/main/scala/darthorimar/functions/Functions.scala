package darthorimar.functions

import darthorimar.renderer.{ExprType, IntType}

object Functions {
  private val functions: Map[String, PartialFunction[List[ExprType], ExprType]] =
    Map(
      "abs" -> {
        case IntType(i)::Nil => IntType(math.abs(i))
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
