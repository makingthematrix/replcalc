package replcalc.eval

sealed trait Error:
  val msg: String

final case class ParsingError(override val msg: String) extends Error
final case class EvaluationError(override val msg: String) extends Error

object Error:
  val Unused: ParsingError = ParsingError("Unused expression")