package replcalc.expressions

import Error.{ParsingError, EvaluationError}
import replcalc.{Dictionary, Parser}

final case class Variable(name: String) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] =
    dict.get(name) match
      case Some(expr) => expr.evaluate(dict)
      case _          => Left(EvaluationError(s"Variable not found: $name"))
  
object Variable extends Parseable[Variable]:
  override def parse(line: String, parser: Parser): ParsedExpr[Variable] =
    if !Dictionary.isValidName(line, true) then 
      None
    else if !parser.dictionary.contains(line) then
      Some(Left(ParsingError(s"Variable not found: $line")))
    else
      Some(Right(Variable(line)))
