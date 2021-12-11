package replcalc.expressions

import Error.ParsingError
import replcalc.{Dictionary, Parser}

final case class Constant(number: Double) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = Right(number)

object Constant extends Parseable[Constant]:
  override def parse(line: String, parser: Parser): ParsedExpr[Constant] =
    line.toDoubleOption match
      case Some(d) => Some(Right(Constant(d)))
      case None    => Some(Left(ParsingError(s"Unable to parse: $line")))
