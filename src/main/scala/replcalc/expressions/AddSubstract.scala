package replcalc.expressions

import scala.annotation.tailrec
import Error.ParsingError
import replcalc.{Dictionary, Parser}

final case class AddSubstract(left: Expression, right: Expression, isSubstraction: Boolean = false) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] =
    for
      l <- left.evaluate(dict)
      r <- right.evaluate(dict)
    yield
      if isSubstraction then l - r else l + r

object AddSubstract extends Parseable[AddSubstract]:
  override def parse(line: String, parser: Parser): ParsedExpr[AddSubstract] =
    val plusIndex = line.lastIndexOf("+")
    val minusIndex = lastBinaryMinus(line)
    val (index, isSubstraction) = if plusIndex > minusIndex then (plusIndex, false) else (minusIndex, true)
    if index > 0 && index < line.length - 1 then
      (for
        lExpr <- parser.parse(line.substring(0, index))
        rExpr <- if (lExpr.isRight) parser.parse(line.substring(index + 1)) else Some(Left(Error.Unused))
      yield (lExpr, rExpr)).map {
        case (Right(l), Right(r)) => Right(AddSubstract(l, r, isSubstraction))
        case (Left(error), _)     => Left(error)
        case (_, Left(error))     => Left(error)
      }
    else
      None

  @tailrec
  private def lastBinaryMinus(line: String): Int =
    line.lastIndexOf("-") match
      case index if index <= 0                                 => -1
      case index if !Parser.isOperator(line.charAt(index - 1)) => index
      case index                                               => lastBinaryMinus(line.substring(0, index))
