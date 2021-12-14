package replcalc.expressions

import Error.{ParsingError, EvaluationError}
import replcalc.{Dictionary, Parser}

final case class Value(name: String) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] =
    dict.get(name) match
      case Some(expr) => expr.evaluate(dict)
      case _          => Left(EvaluationError(s"Value not found: $name"))
  
object Value extends Parseable[Value]:
  override def parse(line: String, parser: Parser): ParsedExpr[Value] =
    if !Dictionary.isValidName(line, true) then 
      None
    else if !parser.dictionary.contains(line) then
      Some(Left(ParsingError(s"Value not found: $line")))
    else
      Some(Right(Value(line)))
