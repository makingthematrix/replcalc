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
    if plusIndex > minusIndex && plusIndex < trimmed.length - 1 then
      Some(
        AddSubstract(
          Expression(trimmed.substring(0, plusIndex)),
          Expression(trimmed.substring(plusIndex + 1))
        )
      )
    else if minusIndex > plusIndex && minusIndex < trimmed.length - 1 then
      Some(
        AddSubstract(
          Expression(trimmed.substring(0, minusIndex)),
          Expression(trimmed.substring(minusIndex + 1)),
          isSubstraction = true
        )
      )
    else
      None

  @tailrec
  private def lastBinaryMinus(text: String): Int =
    val minusIndex = text.lastIndexOf("-")
    if minusIndex <= 0 then 
      -1
    else if !isOperator(text.charAt(minusIndex - 1)) then 
      minusIndex
    else 
      lastBinaryMinus(text.substring(0, minusIndex))
      