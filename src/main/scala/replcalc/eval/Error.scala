package replcalc.eval

enum Error(val msg: String):
  case ParsingError(override val msg: String) extends Error(msg)
  case EvaluationError(override val msg: String) extends Error(msg)

object Error:
  val Unused: ParsingError = ParsingError("Unused expression")