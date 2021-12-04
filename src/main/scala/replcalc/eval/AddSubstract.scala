package replcalc.eval

import scala.annotation.tailrec
import Expression.isOperator

final case class AddSubstract(left: Expression, right: Expression, isSubstraction: Boolean = false) extends Expression:
  override def evaluate: Double =
    if isSubstraction then
      left.evaluate - right.evaluate
    else
      left.evaluate + right.evaluate

object AddSubstract extends Parseable[AddSubstract]:
  override def parse(text: String): Option[AddSubstract] =
    val trimmed = text.trim
    val plusIndex = trimmed.lastIndexOf("+")
    val minusIndex = lastBinaryMinus(text)
    val (index, isSubstraction) = if plusIndex > minusIndex then (plusIndex, false) else (minusIndex, true)
    if index > 0 && index < trimmed.length - 1 then
      Some(
        AddSubstract(
          Expression(trimmed.substring(0, index)),
          Expression(trimmed.substring(index + 1)),
          isSubstraction = isSubstraction
        )
      )
    else
      None

  @tailrec
  private def lastBinaryMinus(text: String): Int =
    text.lastIndexOf("-") match
      case index if index <= 0                          => -1
      case index if !isOperator(text.charAt(index - 1)) => index
      case index                                        => lastBinaryMinus(text.substring(0, index))
      