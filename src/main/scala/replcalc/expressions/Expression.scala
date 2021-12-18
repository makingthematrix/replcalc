package replcalc.expressions

import replcalc.{Dictionary, Parser}
import replcalc.expressions.Error

trait Parseable[T <: Expression]:
  def parse(parser: Parser, line: String): ParsedExpr[T]

trait Expression:
  protected def evaluate(dict: Dictionary): Either[Error, Double]
  final def run(dict: Dictionary): Either[Error, Double] = evaluate(dict).map(Expression.round(_))

object Expression:
  // A magic number. This should be equal to 0.0 but isn't - it's around 1.1E-16 - because of floating point imprecision.
  // To "fix" it, we treat every evaluation result less or equal to this as if it was 0.0.
  private val DIVISION_PRECISION: Double =
   ((1.0/3.0)*3.0)-(1.0/3.0)-(1.0/3.0)-(1.0/3.0)

  inline def isZero(number: Double): Boolean = scala.math.abs(number) <= DIVISION_PRECISION
  inline def round(number: Double): Double = if isZero(number) then 0.0 else number
