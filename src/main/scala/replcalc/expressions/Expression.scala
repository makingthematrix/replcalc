package replcalc.expressions

import replcalc.Parser
import replcalc.expressions.Error.ParsingError

type ParsedExpr[T] = Option[Either[ParsingError, T]]

trait Parseable[T <: Expression]:
  def parse(line: String, parser: Parser): ParsedExpr[T]

trait Expression:
  def evaluate: Either[Error, Double]

