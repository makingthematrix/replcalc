package replcalc

import replcalc.Preprocessor.Flags
import replcalc.expressions.*
import scala.util.chaining.*

trait Parser {
  val dictionary: Dictionary
  def setup(preprocessor: Preprocessor): Unit
  def copy(updates: Map[String, Expression]): Parser
  def parse(line: String): ParsedExpr[Expression]
}

final class ParserImpl(override val dictionary: Dictionary, 
                       private var preprocessor: Option[Preprocessor]) extends Parser:
  self =>
  import Parser.*
  
  override def setup(preprocessor: Preprocessor): Unit =
    this.preprocessor = Some(preprocessor)

  override def copy(updates: Map[String, Expression]): Parser = 
    Parser(dictionary.copy(updates))

  override def parse(line: String): ParsedExpr[Expression] =
    preprocess(line) match
      case Left(error) =>
        Some(Left(error))
      case Right(processed) =>
        // about early returns in Scala: https://makingthematrix.wordpress.com/2021/03/09/many-happy-early-returns/
        object Parsed:
          def unapply(stage: (Parser, String) => ParsedExpr[Expression]): ParsedExpr[Expression] = stage(self, processed)
        stages.collectFirst { case Parsed(expr) => expr }

  private def preprocess(line: String): Either[Error, String] =
    preprocessor match
      case Some(pre) => pre.process(line)
      case None      => Left(Error.PreprocessorError("The preprocessor is not set up"))

object Parser:
  def apply(dictionary: Dictionary = Dictionary()): Parser =
    new ParserImpl(dictionary, None).tap { parser =>
      parser.setup(Preprocessor(parser))
    }

  def isOperator(char: Char, additionalAllowed: Char*): Boolean = 
    operators.contains(char) || additionalAllowed.contains(char)

  val operators: Set[Char] = Set('+', '-', '*', '/', ',')

  val stages: Seq[(Parser, String) => ParsedExpr[Expression]] =
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
