package replcalc.eval

final case class UnaryMinus(innerExpr: Expression) extends Expression:
  override def evaluate: Double = -innerExpr.evaluate
  
object UnaryMinus extends Parseable[UnaryMinus]:
  def parse(text: String): Option[UnaryMinus] =
    if (text.charAt(0) == '-') 
      Some(UnaryMinus(Expression(text.substring(1))))
    else 
      None
