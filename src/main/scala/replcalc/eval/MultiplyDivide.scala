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
    val (index, isDivision) = if mulIndex > divIndex then (mulIndex, false) else (divIndex, true)
    if index > 0 && index < trimmed.length - 1 then
      Some(
        MultiplyDivide(
          Expression(trimmed.substring(0, index)),
          Expression(trimmed.substring(index + 1)),
          isDivision = isDivision
        )
      )
    else
      None
