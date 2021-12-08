package replcalc.eval

import scala.annotation.tailrec
import Error.*

trait Expression:
  def evaluate: Either[Error, Double]

type ParsedExpr[T] = Option[Either[ParsingError, T]]

// feels good, might delete later
trait Parseable[T <: Expression]:
  def parse(line: String): ParsedExpr[T]

object Expression extends Parseable[Expression]:
  override def parse(line: String): ParsedExpr[Expression] =
    val trimmed = line.trim
    // about early returns in Scala: https://makingthematrix.wordpress.com/2021/03/09/many-happy-early-returns/
    object Parsed:
      def unapply(stage: String => ParsedExpr[Expression]): ParsedExpr[Expression] = stage(trimmed)
    stages.collectFirst { case Parsed(expression) => expression }

  inline def isOperator(char: Char): Boolean = operators.contains(char)

  private val operators: Set[Char] = Set('+', '-', '*', '/')

  private val stages: Seq[String => ParsedExpr[Expression]] = Seq(
    Assignment.parse,
    AddSubstract.parse,
    MultiplyDivide.parse,
    UnaryMinus.parse,
    Constant.parse
  )
