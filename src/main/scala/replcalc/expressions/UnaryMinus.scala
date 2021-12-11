package replcalc.expressions

import Error.ParsingError
import replcalc.{Dictionary, Parser}

final case class UnaryMinus(innerExpr: Expression) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = innerExpr.evaluate(dict).map(-_)
  
object UnaryMinus extends Parseable[UnaryMinus]:
  override def parse(line: String, parser: Parser): ParsedExpr[UnaryMinus] =
    if line.length > 1 && line.charAt(0) == '-' then
      parser.parse(line.substring(1)).map(_.map(UnaryMinus.apply))
    else 
      None
