package replcalc.eval

import Error.ParsingError

final case class Value(name: String, expr: Expression) extends Expression:
  override def evaluate: Either[Error, Double] = expr.evaluate
  
object Value extends Parseable[Value]:
  override def parse(line: String, parser: Parser): ParsedExpr[Value] =
    if !isValidValueName(line) then 
      None
    else  
      parser.getValue(line) match
        case Some(expr) => Some(Right(Value(line, expr)))
        case None       => Some(Left(ParsingError(s"Value not found: $line")))

  def isValidValueName(name: String): Boolean =
    name.nonEmpty &&
      (name(0).isLetter || name(0) == '_') &&
      name.substring(1).forall(ch => ch.isLetterOrDigit || ch == '_')