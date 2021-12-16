package replcalc.expressions

import Error.{ParsingError, EvaluationError}
import replcalc.{Dictionary, Parser}

final case class Variable(name: String) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] =
    dict
      .get(name)
      .map(_.evaluate(dict))
      .getOrElse(Left(EvaluationError(s"Variable not found: $name")))
  
object Variable extends Parseable[Variable]:
  override def parse(parser: Parser, line: String): ParsedExpr[Variable] =
    if !Dictionary.isValidName(line, true) then 
      ParsedExpr.empty
    else if !parser.dictionary.contains(line) then
      ParsedExpr.error(s"Variable not found: $line")
    else
      ParsedExpr(Variable(line))
