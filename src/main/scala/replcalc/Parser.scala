package replcalc

import replcalc.Preprocessor.Flags
import replcalc.expressions.*

final class Parser(private val dict: Dictionary = Dictionary(), preprocessorFlags: Flags = Flags.AllTrue):
  self =>
  import Parser.*

  private lazy val pre = Preprocessor(this, preprocessorFlags)
  
  def dictionary: Dictionary = dict
  def preprocessor: Preprocessor = pre 

  def parse(line: String): ParsedExpr[Expression] =
    pre.process(line) match
      case Left(error) =>
        Some(Left(error))
      case Right(processed) =>
        // about early returns in Scala: https://makingthematrix.wordpress.com/2021/03/09/many-happy-early-returns/
        object Parsed:
          def unapply(stage: (String, Parser) => ParsedExpr[Expression]): ParsedExpr[Expression] = stage(processed, self)
        stages.collectFirst { case Parsed(expr) => expr }

object Parser:
  inline def isOperator(char: Char): Boolean = operators.contains(char)

  private val operators: Set[Char] = Set('+', '-', '*', '/', ',')

  private val stages: Seq[(String, Parser) => ParsedExpr[Expression]] =
    Seq(
      FunctionAssignment.parse,
      ValueAssignment.parse,
      AddSubstract.parse,
      MultiplyDivide.parse,
      UnaryMinus.parse,
      Function.parse,
      Value.parse,
      Constant.parse
    )
