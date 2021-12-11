package replcalc.expressions

import Error.{ParsingError, EvaluationError}
import replcalc.{Dictionary, Parser}

final case class Value(name: String) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] =
    dict.get(name) match {
      case Some(expr) => expr.evaluate(dict)
      case None       => Left(EvaluationError(s"Evaluation error: Value not found: $name"))
    }

  
object Value extends Parseable[Value]:
  override def parse(line: String, parser: Parser): ParsedExpr[Value] =
    if !isValidValueName(line, true) then 
      None
    else if !parser.containsValue(line) then
      Some(Left(ParsingError(s"Parsing error: Value not found: $line")))
    else
      Some(Right(Value(line)))

  def isValidValueName(name: String, canBeSpecial: Boolean = false): Boolean =
    name.nonEmpty &&
      (name(0).isLetter || name(0) == '_' || (canBeSpecial && name(0) == '$')) &&
      name.substring(1).forall(ch => ch.isLetterOrDigit || ch == '_')