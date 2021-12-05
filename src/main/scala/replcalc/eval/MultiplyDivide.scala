package replcalc.eval

final case class MultiplyDivide(left: Expression, right: Expression, isDivision: Boolean = false) extends Expression:
  override def evaluate: Option[Double] =
    for
      l <- left.evaluate
      r <- right.evaluate
    yield
      if isDivision then l / r else l * r

object MultiplyDivide extends Parseable[MultiplyDivide]:
  override def parse(text: String): Option[MultiplyDivide] =
    val trimmed = text.trim
    val mulIndex = trimmed.lastIndexOf("*")
    val divIndex = trimmed.lastIndexOf("/")
    val (index, isDivision) = if mulIndex > divIndex then (mulIndex, false) else (divIndex, true)
    if index > 0 && index < trimmed.length - 1 then
      for 
        left  <- Expression.parse(trimmed.substring(0, index))
        right <- Expression.parse(trimmed.substring(index + 1))
      yield
        MultiplyDivide(left, right, isDivision)
    else
      None
