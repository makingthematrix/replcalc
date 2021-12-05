package replcalc.eval

import scala.annotation.tailrec

trait Expression:
  def evaluate: Option[Double]

object Expression extends Parseable[Expression]:
  def parse(text: String): Option[Expression] =
    val trimmed = text.trim
    AddSubstract.parse(trimmed)
      .orElse(MultiplyDivide.parse(trimmed))
      .orElse(UnaryMinus.parse(trimmed))
      .orElse(Constant.parse(trimmed))
  
  val operators: Set[Char] = Set('+', '-', '*', '/')
  
  inline def isOperator(char: Char): Boolean = operators.contains(char)