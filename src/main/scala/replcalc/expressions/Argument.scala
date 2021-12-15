package replcalc.expressions

import replcalc.Dictionary
import replcalc.expressions.Error.EvaluationError

final case class Argument(name: String) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] =
    dict.get(name) match
      case Some(expr) => expr.evaluate(dict)
      case _          => Left(EvaluationError(s"Value not found for the argument: $name"))
