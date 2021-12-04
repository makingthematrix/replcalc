package replcalc.eval

final case class UnaryMinus(innerExpr: Expression) extends Expression:
  override def evaluate: Double = -innerExpr.evaluate
  
object UnaryMinus extends Parseable[UnaryMinus]:
  override def parse(text: String): Option[UnaryMinus] =
    val trimmed = text.trim
    if trimmed.charAt(0) == '-' then
      Some(UnaryMinus(Expression(trimmed.substring(1))))
    else 
      None
