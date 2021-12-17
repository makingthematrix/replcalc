package replcalc.expressions

import Error.*
import replcalc.{Dictionary, Parser, Preprocessor}
import scala.util.chaining.*

final case class FunctionAssignment(name: String, argNames: Seq[String], expr: Expression) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = expr.evaluate(dict)

object FunctionAssignment extends Parseable[FunctionAssignment]:
  override def parse(parser: Parser, line: String): ParsedExpr[FunctionAssignment] =
    if !line.contains("=") then
      ParsedExpr.empty
    else
      val assignIndex   = line.indexOf('=')
      val assignmentStr = line.substring(0, assignIndex)
      Preprocessor.findParens(assignmentStr, functionParens = true).flatMap {
        case Left(error) =>
          ParsedExpr.error(error)
        case Right((_, closing)) if closing + 1 < assignmentStr.length =>
          ParsedExpr.error(s"Unrecognized chunk of a function expression: ${line.substring(closing + 1)}")
        case Right((opening, closing)) =>
          val functionName  = assignmentStr.substring(0, opening)
          val arguments     = assignmentStr.substring(opening + 1, closing)
          val expressionStr = line.substring(assignIndex + 1)
          parseAssignment(parser, functionName, arguments, expressionStr)
      }

  private def parseAssignment(parser: Parser, functionName: String, arguments: String, expressionStr: String): ParsedExpr[FunctionAssignment] =
    if !Dictionary.isValidName(functionName) then
      ParsedExpr.error(s"Invalid function name: $functionName")
    else if parser.dictionary.contains(functionName) then
      ParsedExpr.error(s"The function already exists: $functionName")
    else
      val argNames = arguments.split(",").map(_.trim).filter(_.nonEmpty).toSeq
      val errors   = argNames.collect { case argName if !Dictionary.isValidName(argName) => argName }
      if errors.nonEmpty then
        ParsedExpr.error(s"Invalid argument(s): ${errors.mkString(", ")}")
      else
        val argsMap = argNames.map(name => name -> Variable(name)).toMap
        parser
          .copy(argsMap)
          .parse(expressionStr)
          .happyPath { expression =>
            FunctionAssignment(functionName, argNames, expression).pipe { assignment =>
              parser.dictionary.add(functionName, assignment)
              ParsedExpr(assignment)
            }
          }
          .errorIfEmpty(s"Unable to parse: $expressionStr")
