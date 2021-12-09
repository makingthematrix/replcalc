package replcalc.eval

import Error.ParsingError

final case class Constant(number: Double) extends Expression:
  override def evaluate: Either[Error, Double] = Right(number)

object Constant extends Parseable[Constant]:
  override def parse(line: String, dict: Dictionary): ParsedExpr[Constant] =
    line.trim.toDoubleOption match
      case Some(d) => Some(Right(Constant(d)))
      case None    => Some(Left(ParsingError(s"Unable to parse: $line")))
