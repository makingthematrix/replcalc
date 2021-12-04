package replcalc.eval

import scala.annotation.tailrec

trait Expression:
  def evaluate: Double

object Expression:
  def apply(text: String): Expression =
    val trimmed = text.trim
    AddSubstract.parse(trimmed)
      .orElse(MultiplyDivide.parse(trimmed))
      .orElse(UnaryMinus.parse(trimmed))
      .orElse(Constant.parse(trimmed))
      .getOrElse(Constant(Double.NaN))
  
  val operators: Set[Char] = Set('+', '-', '*', '/')