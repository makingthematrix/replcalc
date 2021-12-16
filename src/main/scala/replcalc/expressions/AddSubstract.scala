package replcalc.expressions

import scala.annotation.tailrec
import Error.ParsingError
import replcalc.{Dictionary, Parser}
import replcalc.Parser.isOperator

final case class AddSubstract(left: Expression, right: Expression, isSubstraction: Boolean = false) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] =
    for
      lResult <- left.evaluate(dict)
      rResult <- right.evaluate(dict)
    yield
      if isSubstraction then 
        lResult - rResult 
      else 
        lResult + rResult

object AddSubstract extends Parseable[AddSubstract]:
  override def parse(parser: Parser, line: String): ParsedExpr[AddSubstract] =
    val plusIndex  = line.lastIndexOf("+")
    val minusIndex = lastBinaryMinus(line)
    val (index, isSubstraction) = if plusIndex > minusIndex then (plusIndex, false) else (minusIndex, true)
    if index <= 0 || index >= line.length - 1 then
      ParsedExpr.empty
    else
      val innerExpressions =
        for
          lExpr <- parser.parse(line.substring(0, index))
          rExpr <- if (lExpr.isRight) parser.parse(line.substring(index + 1)) else ParsedExpr.unused
        yield (lExpr, rExpr)
      innerExpressions.map {
        case (Left(error), _)     => Left(error)
        case (_, Left(error))     => Left(error)
        case (Right(l), Right(r)) => Right(AddSubstract(l, r, isSubstraction))
      }
    
  @tailrec
  private def lastBinaryMinus(line: String): Int =
    line.lastIndexOf("-") match
      case index if index <= 0                          => -1
      case index if !isOperator(line.charAt(index - 1)) => index
      case index                                        => lastBinaryMinus(line.substring(0, index))
