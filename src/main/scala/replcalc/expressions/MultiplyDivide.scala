package replcalc.expressions

import Error.*
import replcalc.{Dictionary, Parser}

final case class MultiplyDivide(left: Expression, right: Expression, isDivision: Boolean = false) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] =
    val innerResults =
      for
        lResult <- left.evaluate(dict)
        rResult <- right.evaluate(dict)
      yield (lResult, rResult)
    innerResults.flatMap {
      case (l, r) if isDivision && r == 0.0 => Left(EvaluationError(s"Division by zero: $l / $r"))
      case (l, r) if isDivision             => Right(l / r)
      case (l, r)                           => Right(l * r)
    }

object MultiplyDivide extends Parseable[MultiplyDivide]:
  override def parse(line: String, parser: Parser): ParsedExpr[MultiplyDivide] =
    val mulIndex = line.lastIndexOf("*")
    val divIndex = line.lastIndexOf("/")
    val (index, isDivision) = if mulIndex > divIndex then (mulIndex, false) else (divIndex, true)
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
        case (Right(l), Right(r)) => Right(MultiplyDivide(l, r, isDivision))
      }
