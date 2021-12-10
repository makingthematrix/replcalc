package replcalc

import replcalc.expressions.*
import replcalc.{Dictionary, Preprocessor}

final class Parser(dict: Dictionary = Dictionary()):
  self =>
  import Parser.*

  private lazy val pre = Preprocessor(this)
  
  // for tests
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

  inline def getValue(name: String): Option[Expression] = dict.get(name)

  inline def addValue(name: String, expr: Expression): Boolean = dict.add(name, expr)

  inline def containsValue(name: String): Boolean = dict.contains(name)

object Parser:
  inline def isOperator(char: Char): Boolean = operators.contains(char)

  private val operators: Set[Char] = Set('+', '-', '*', '/')

  private val stages: Seq[(String, Parser) => ParsedExpr[Expression]] =
    Seq(
      Assignment.parse,
      AddSubstract.parse,
      MultiplyDivide.parse,
      UnaryMinus.parse,
      Value.parse,
      Constant.parse
    )
