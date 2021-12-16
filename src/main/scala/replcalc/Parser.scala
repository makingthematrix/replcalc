package replcalc

import replcalc.Preprocessor.Flags
import replcalc.expressions.*

final class Parser(val dictionary: Dictionary = Dictionary(), preprocessorFlags: Flags = Flags.AllFlagsOn):
  self =>
  import Parser.*

  lazy val preprocessor: Preprocessor = Preprocessor(this, preprocessorFlags)

  def parse(line: String): ParsedExpr[Expression] =
    preprocessor.process(line) match
      case Left(error) =>
        Some(Left(error))
      case Right(processed) =>
        // about early returns in Scala: https://makingthematrix.wordpress.com/2021/03/09/many-happy-early-returns/
        object Parsed:
          def unapply(stage: (Parser, String) => ParsedExpr[Expression]): ParsedExpr[Expression] = stage(self, processed)
        stages.collectFirst { case Parsed(expr) => expr }

object Parser:
  inline def isOperator(char: Char): Boolean = operators.contains(char)

  private val operators: Set[Char] = Set('+', '-', '*', '/', ',')

  private val stages: Seq[(Parser, String) => ParsedExpr[Expression]] =
    Seq(
      FunctionAssignment.parse,
      Assignment.parse,
      AddSubstract.parse,
      MultiplyDivide.parse,
      UnaryMinus.parse,
      Function.parse,
      Variable.parse,
      Constant.parse,
      Failure.parse
    )
