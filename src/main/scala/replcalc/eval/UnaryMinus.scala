package replcalc.eval

import Error.ParsingError

final case class UnaryMinus(innerExpr: Expression) extends Expression:
  override def evaluate: Either[Error, Double] = innerExpr.evaluate.map(-_)
  
object UnaryMinus extends Parseable[UnaryMinus]:
  override def parse(line: String, parser: Parser): ParsedExpr[UnaryMinus] =
    if line.length > 1 && line.charAt(0) == '-' then
      parser.parse(line.substring(1)).map(_.map(UnaryMinus.apply))
    else 
      None
