package replcalc.eval

final case class Constant(number: Double) extends Expression:
  override def evaluate: Double = number

object Constant extends Parseable[Constant]:
  override def parse(text: String): Option[Constant] =
    text.trim.toDoubleOption.map(Constant.apply)
    