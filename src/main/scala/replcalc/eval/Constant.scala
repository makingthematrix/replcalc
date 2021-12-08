package replcalc.eval

import Error.ParsingError

final case class Constant(number: Double) extends Expression:
  override def evaluate: Either[Error, Double] = Right(number)

object Constant extends Parseable[Constant]:
  override def parse(text: String): ParsedExpr[Constant] =
    text.trim.toDoubleOption match 
      case Some(d) => Some(Right(Constant(d)))
      case None    => Some(Left(ParsingError(s"Unable to parse $text")))
