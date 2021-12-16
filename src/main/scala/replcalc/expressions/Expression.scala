package replcalc.expressions

import replcalc.{Dictionary, Parser}
import replcalc.expressions.Error

trait Parseable[T <: Expression]:
  def parse(parser: Parser, line: String): ParsedExpr[T]

trait Expression:
  def evaluate(dict: Dictionary): Either[Error, Double]

