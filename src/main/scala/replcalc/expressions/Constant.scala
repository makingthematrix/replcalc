package replcalc.expressions

import Error.ParsingError
import replcalc.{Dictionary, Parser}

final case class Constant(number: Double) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = Right(number)

object Constant extends Parseable[Constant]:
  override def parse(line: String, parser: Parser): ParsedExpr[Constant] =
    line
      .toDoubleOption
      .map(number => Right(Constant(number)))
