package replcalc.expressions

import Error.*
import replcalc.{Dictionary, Parser}
import Expression.isZero

final case class MultiplyDivide(left: Expression, right: Expression, isDivision: Boolean = false) extends Expression:
  override protected def evaluate(dict: Dictionary): Either[Error, Double] =
    val innerResults =
      for
        lResult <- left.run(dict)
        rResult <- right.run(dict)
      yield (lResult, rResult)
    innerResults.flatMap {
      case (l, r) if isDivision && isZero(r) => Left(EvaluationError(s"Division by zero: $l / $r"))
      case (l, r) if isDivision              => Right(l / r)
      case (l, r)                            => Right(l * r)
    }

object MultiplyDivide extends Parseable[MultiplyDivide]:
  override def parse(parser: Parser, line: String): ParsedExpr[MultiplyDivide] =
    val mulIndex = line.lastIndexOf("*")
    val divIndex = line.lastIndexOf("/")
    val (index, isDivision) = if mulIndex > divIndex then (mulIndex, false) else (divIndex, true)
    if index <= 0 || index >= line.length - 1 then
      ParsedExpr.empty
    else
      val innerExpressions =
        for
          lExpr <- parser.parse(line.substring(0, index))
          rExpr <- if lExpr.isRight then parser.parse(line.substring(index + 1)) else ParsedExpr.unused
        yield (lExpr, rExpr)
      innerExpressions.map {
        case (Left(error), _)     => Left(error)
        case (_, Left(error))     => Left(error)
        case (Right(l), Right(r)) => Right(MultiplyDivide(l, r, isDivision))
      }
