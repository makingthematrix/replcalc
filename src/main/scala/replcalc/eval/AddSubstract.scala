package replcalc.eval

import scala.annotation.tailrec
import Expression.isOperator
import Error.ParsingError

final case class AddSubstract(left: Expression, right: Expression, isSubstraction: Boolean = false) extends Expression:
  override def evaluate: Either[Error, Double] =
    for
      l <- left.evaluate
      r <- right.evaluate
    yield
      if isSubstraction then l - r else l + r

object AddSubstract extends Parseable[AddSubstract]:
  override def parse(text: String): ParsedExpr[AddSubstract] =
    val trimmed = text.trim
    val plusIndex = trimmed.lastIndexOf("+")
    val minusIndex = lastBinaryMinus(text)
    val (index, isSubstraction) = if plusIndex > minusIndex then (plusIndex, false) else (minusIndex, true)
    if index > 0 && index < trimmed.length - 1 then
      (for
        lExpr <- Expression.parse(trimmed.substring(0, index))
        rExpr <- if (lExpr.isRight) Expression.parse(trimmed.substring(index + 1)) else Some(Left(Error.Unused))
      yield (lExpr, rExpr)).map {
        case (Right(l), Right(r)) => Right(AddSubstract(l, r, isSubstraction))
        case (Left(error), _)     => Left(error)
        case (_, Left(error))     => Left(error)
      }
    else
      None

  @tailrec
  private def lastBinaryMinus(text: String): Int =
    text.lastIndexOf("-") match
      case index if index <= 0                         => -1
      case index if !isOperatorBefore(text, index - 1) => index
      case index                                       => lastBinaryMinus(text.substring(0, index))

  @tailrec
  private def isOperatorBefore(text: String, index: Int): Boolean =
    if index < 0 then false
    else
      val ch = text(index)
      if ch.isWhitespace then isOperatorBefore(text, index - 1) else isOperator(ch)