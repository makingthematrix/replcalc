package replcalc.eval

import scala.annotation.tailrec

trait Expression:
  def evaluate: Option[Double]

object Expression extends Parseable[Expression]:
  override def parse(text: String): Option[Expression] =
    val trimmed = text.trim
    // about early returns in Scala: https://makingthematrix.wordpress.com/2021/03/09/many-happy-early-returns/
    object Parsed:
      def unapply[T <: Expression](stage: String => Option[T]): Option[T] = stage(trimmed)
    stages.collectFirst { case Parsed(expression) => expression }

  inline def isOperator(char: Char): Boolean = operators.contains(char)

  private val operators: Set[Char] = Set('+', '-', '*', '/')

  private val stages = Seq(
    AddSubstract.parse,
    MultiplyDivide.parse,
    UnaryMinus.parse,
    Constant.parse
  )
