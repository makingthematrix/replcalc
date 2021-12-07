package replcalc.eval

final case class UnaryMinus(innerExpr: Expression) extends Expression:
  override def evaluate: Either[Error, Double] = innerExpr.evaluate.map(-_)
  
object UnaryMinus extends Parseable[UnaryMinus]:
  override def parse(text: String): ParsedExpr[UnaryMinus] =
    val trimmed = text.trim
    if trimmed.length > 1 && trimmed.charAt(0) == '-' then
      Expression.parse(trimmed.substring(1)).map(_.map(UnaryMinus.apply))
    else 
      None
