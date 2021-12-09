package replcalc.eval

import scala.annotation.tailrec
import Error.ParsingError

final case class AddSubstract(left: Expression, right: Expression, isSubstraction: Boolean = false) extends Expression:
  override def evaluate: Either[Error, Double] =
    for
      l <- left.evaluate
      r <- right.evaluate
    yield
      if isSubstraction then l - r else l + r

object AddSubstract extends Parseable[AddSubstract]:
  override def parse(line: String, dictionary: Dictionary): ParsedExpr[AddSubstract] =
    val trimmed = line.trim
    val plusIndex = trimmed.lastIndexOf("+")
    val minusIndex = lastBinaryMinus(line)
    val (index, isSubstraction) = if plusIndex > minusIndex then (plusIndex, false) else (minusIndex, true)
    if index > 0 && index < trimmed.length - 1 then
      (for
        lExpr <- Parser.parse(trimmed.substring(0, index), dictionary)
        rExpr <- if (lExpr.isRight) Parser.parse(trimmed.substring(index + 1), dictionary) else Some(Left(Error.Unused))
      yield (lExpr, rExpr)).map {
        case (Right(l), Right(r)) => Right(AddSubstract(l, r, isSubstraction))
        case (Left(error), _)     => Left(error)
        case (_, Left(error))     => Left(error)
      }
    else
      None

  @tailrec
  private def lastBinaryMinus(str: String): Int =
    str.lastIndexOf("-") match
      case index if index <= 0                         => -1
      case index if !isOperatorBefore(str, index - 1) => index
      case index                                       => lastBinaryMinus(str.substring(0, index))

  @tailrec
  private def isOperatorBefore(str: String, index: Int): Boolean =
    if index < 0 then false
    else
      val ch = str(index)
      if ch.isWhitespace then isOperatorBefore(str, index - 1) else Parser.isOperator(ch)