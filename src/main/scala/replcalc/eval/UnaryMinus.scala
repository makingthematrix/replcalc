package replcalc.eval

final case class UnaryMinus(innerExpr: Expression) extends Expression:
  override def evaluate: Option[Double] = innerExpr.evaluate.map(-_)
  
object UnaryMinus extends Parseable[UnaryMinus]:
  override def parse(text: String): Option[UnaryMinus] =
    val trimmed = text.trim
    if trimmed.length > 1 && trimmed.charAt(0) == '-' then
      Expression.parse(trimmed.substring(1)).map(UnaryMinus.apply)
    else 
      None
