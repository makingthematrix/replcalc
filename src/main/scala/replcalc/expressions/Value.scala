package replcalc.expressions

import Error.ParsingError
import replcalc.Parser

final case class Value(name: String, expr: Expression) extends Expression:
  override def evaluate: Either[Error, Double] = expr.evaluate
  
object Value extends Parseable[Value]:
  override def parse(line: String, parser: Parser): ParsedExpr[Value] =
    if !isValidValueName(line, true) then 
      None
    else  
      parser.getValue(line) match
        case Some(expr) => Some(Right(Value(line, expr)))
        case None       => Some(Left(ParsingError(s"Value not found: $line")))

  def isValidValueName(name: String, canBeSpecial: Boolean = false): Boolean =
    name.nonEmpty &&
      (name(0).isLetter || name(0) == '_' || (canBeSpecial && name(0) == '$')) &&
      name.substring(1).forall(ch => ch.isLetterOrDigit || ch == '_')