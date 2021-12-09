package replcalc.eval

import Error.ParsingError

final case class UnaryMinus(innerExpr: Expression) extends Expression:
  override def evaluate: Either[Error, Double] = innerExpr.evaluate.map(-_)
  
object UnaryMinus extends Parseable[UnaryMinus]:
  override def parse(line: String, dict: Dictionary): ParsedExpr[UnaryMinus] =
    val trimmed = line.trim
    if trimmed.length > 1 && trimmed.charAt(0) == '-' then
      Parser.parse(trimmed.substring(1), dict).map(_.map(UnaryMinus.apply))
    else 
      None
