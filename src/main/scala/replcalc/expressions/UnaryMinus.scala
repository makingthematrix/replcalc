package replcalc.expressions

import Error.ParsingError
import replcalc.{Dictionary, Parser, expressions}

final case class UnaryMinus(innerExpr: Expression) extends Expression:
  override protected def evaluate(dict: Dictionary): Either[Error, Double] = innerExpr.run(dict).map(-_)
  
object UnaryMinus extends Parseable[UnaryMinus]:
  override def parse(parser: Parser, line: String): ParsedExpr[UnaryMinus] =
    if line.length <= 1 || line.head != '-' then
      ParsedExpr.empty
    else
      parser
        .parse(line.substring(1))
        .happyPath(expr => ParsedExpr(UnaryMinus(expr)))
