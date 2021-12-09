package replcalc.eval

import Error.ParsingError

final case class Value(name: String, expr: Expression) extends Expression:
  override def evaluate: Either[Error, Double] = expr.evaluate
  
object Value extends Parseable[Value]:
  override def parse(line: String, dict: Dictionary): ParsedExpr[Value] =
    val trimmed = line.trim
    if !isValidValueName(trimmed) then 
      None
    else  
      dict.get(trimmed) match
        case Some(expr) => Some(Right(Value(trimmed, expr)))
        case None       => Some(Left(ParsingError(s"Value not found: $trimmed")))

  def isValidValueName(name: String): Boolean =
    name.nonEmpty &&
      (name(0).isLetter || name(0) == '_') &&
      name.substring(1).forall(ch => ch.isLetterOrDigit || ch == '_')