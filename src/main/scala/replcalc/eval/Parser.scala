package replcalc.eval

import replcalc.eval.Error.ParsingError

type ParsedExpr[T] = Option[Either[ParsingError, T]]

trait Parseable[T <: Expression]:
  def parse(line: String, dict: Dictionary): ParsedExpr[T]

object Parser extends Parseable[Expression]:
  override def parse(line: String, dict: Dictionary): ParsedExpr[Expression] =
    val processed = pre.process(line)
    // about early returns in Scala: https://makingthematrix.wordpress.com/2021/03/09/many-happy-early-returns/
    object Parsed:
      def unapply(stage: (String, Dictionary) => ParsedExpr[Expression]): ParsedExpr[Expression] = stage(processed, dict)
    stages.collectFirst { case Parsed(expr) => expr }
      
  private val pre = Preprocessor()  
    
  inline def isOperator(char: Char): Boolean = operators.contains(char)

  private val operators: Set[Char] = Set('+', '-', '*', '/')

  private val stages: Seq[(String, Dictionary) => ParsedExpr[Expression]] =
    Seq(Assignment.parse,
        AddSubstract.parse,
        MultiplyDivide.parse,
        UnaryMinus.parse,
        Value.parse,
        Constant.parse)
