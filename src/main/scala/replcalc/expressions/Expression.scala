package replcalc.expressions

import replcalc.{Dictionary, Parser}
import replcalc.expressions.Error

trait Parseable[T <: Expression]:
  def parse(line: String, parser: Parser): ParsedExpr[T]

trait Expression:
  def evaluate(dict: Dictionary): Either[Error, Double]

