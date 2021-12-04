package replcalc.eval

final case class MultiplyDivide(left: Expression, right: Expression, isDivision: Boolean = false) extends Expression:
  override def evaluate: Double =
    if isDivision then
      left.evaluate / right.evaluate
    else
      left.evaluate * right.evaluate

object MultiplyDivide extends Parseable[MultiplyDivide]:
  override def parse(text: String): Option[MultiplyDivide] =
    val trimmed = text.trim
    val mulIndex = trimmed.lastIndexOf("*")
    val divIndex = trimmed.lastIndexOf("/")
    if mulIndex > divIndex && mulIndex < trimmed.length - 1 then
      Some(
        MultiplyDivide(
          Expression(trimmed.substring(0, mulIndex)),
          Expression(trimmed.substring(mulIndex + 1))
        )
      )
    else if divIndex > mulIndex && divIndex < trimmed.length - 1 then
      Some(
        MultiplyDivide(
          Expression(trimmed.substring(0, divIndex)),
          Expression(trimmed.substring(divIndex + 1)),
          isDivision = true
        )
      )
    else
      None
      
