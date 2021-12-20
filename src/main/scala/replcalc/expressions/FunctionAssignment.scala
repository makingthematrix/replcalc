package replcalc.expressions

import Error.*
import replcalc.Preprocessor.{ParsedFunction, LineSide}
import replcalc.{Dictionary, Parser, Preprocessor}
import scala.util.chaining.*

final case class FunctionAssignment(name: String, argNames: Seq[String], expr: Expression) extends Expression:
  override protected def evaluate(dict: Dictionary): Either[Error, Double] = expr.run(dict)

object FunctionAssignment extends Parseable[FunctionAssignment]:
  override def parse(parser: Parser, line: String): ParsedExpr[FunctionAssignment] =
    if !line.contains("=") then
      ParsedExpr.empty
    else
      val assignIndex   = line.indexOf('=')
      val assignmentStr = line.substring(0, assignIndex)
      Preprocessor.parseFunction(assignmentStr, LineSide.Left).flatMap {
        case Left(error) =>
          ParsedExpr.error(error)
        case Right(ParsedFunction(name, _)) if parser.dictionary.contains(name) =>
          ParsedExpr.error(s"The function already exists: $name")
        case Right(ParsedFunction(name, arguments)) =>
          val expressionStr = line.substring(assignIndex + 1)
          parseAssignment(parser, name, arguments, expressionStr)
      }

  private def parseAssignment(parser: Parser, name: String, arguments: Seq[String], expressionStr: String): ParsedExpr[FunctionAssignment] =
    parser
      .copy(arguments.map(arg => arg -> Variable(arg)).toMap)
      .parse(expressionStr)
      .happyPath { expression =>
        FunctionAssignment(name, arguments, expression).pipe { assignment =>
          parser.dictionary.add(name, assignment)
          ParsedExpr(assignment)
        }
      }
      .errorIfEmpty(s"Unable to parse: $expressionStr")
