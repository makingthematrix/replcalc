package replcalc.eval

final case class AddSubstract(left: Expression, right: Expression, isSubstraction: Boolean = false) extends Expression:
  override def evaluate: Double =
    if (isSubstraction)
      left.evaluate - right.evaluate
    else
      left.evaluate + right.evaluate

object AddSubstract extends Parseable[AddSubstract]:
  def parse(text: String): Option[AddSubstract] =
    val trimmed = text.trim
    val plusIndex = trimmed.lastIndexOf("+")
    val minusIndex = trimmed.lastIndexOf("-")
    if (plusIndex > minusIndex && plusIndex < trimmed.length - 1)
      Some(
        AddSubstract(
          Expression(trimmed.substring(0, plusIndex)),
          Expression(trimmed.substring(plusIndex + 1))
        )
      )
    else if (minusIndex > plusIndex && minusIndex < trimmed.length - 1)
      Some(
        AddSubstract(
          Expression(trimmed.substring(0, minusIndex)),
          Expression(trimmed.substring(minusIndex + 1)),
          isSubstraction = true
        )
      )
    else
      None
      