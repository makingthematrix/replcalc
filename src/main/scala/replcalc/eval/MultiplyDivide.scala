package replcalc.eval

import Error.*

final case class MultiplyDivide(left: Expression, right: Expression, isDivision: Boolean = false) extends Expression:
  override def evaluate: Either[Error, Double] =
    (for
      lResult <- left.evaluate
      rResult <- right.evaluate
    yield (lResult, rResult)).flatMap {
      case (l, r) if isDivision && r == 0.0 => Left(EvaluationError(s"Division by zero: $l / $r"))
      case (l, r) if isDivision             => Right(l / r)
      case (l, r)                           => Right(l * r)
    }

object MultiplyDivide extends Parseable[MultiplyDivide]:
  override def parse(line: String, dictionary: Dictionary): ParsedExpr[MultiplyDivide] =
    val trimmed = line.trim
    val mulIndex = trimmed.lastIndexOf("*")
    val divIndex = trimmed.lastIndexOf("/")
    val (index, isDivision) = if mulIndex > divIndex then (mulIndex, false) else (divIndex, true)
    if index > 0 && index < trimmed.length - 1 then
      (for
        lExpr <- Parser.parse(trimmed.substring(0, index), dictionary)
        rExpr <- if (lExpr.isRight) Parser.parse(trimmed.substring(index + 1), dictionary) else Some(Left(Error.Unused))
      yield (lExpr, rExpr)).map {
        case (Right(l), Right(r)) => Right(MultiplyDivide(l, r, isDivision))
        case (Left(error), _)     => Left(error)
        case (_, Left(error))     => Left(error)
      }
    else
      None
