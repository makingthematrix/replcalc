package replcalc.expressions

import Error.*
import replcalc.{Dictionary, Parser, Preprocessor}
import scala.util.chaining.*

final case class FunctionAssignment(name: String, argNames: Seq[String], expr: Expression) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = expr.evaluate(dict)

object FunctionAssignment extends Parseable[FunctionAssignment]:
  override def parse(line: String, parser: Parser): ParsedExpr[FunctionAssignment] =
    if !line.contains("=") then
      None
    else
      val assignIndex = line.indexOf('=')
      val assignmentStr = line.substring(0, assignIndex)
      Preprocessor.findParens(assignmentStr, functionParens = true).map {
        case Left(error) =>
          Left(error)
        case Right((opening, closing)) =>
          val functionName  = assignmentStr.substring(0, opening)
          val arguments     = assignmentStr.substring(opening + 1, closing)
          val expressionStr = line.substring(assignIndex + 1)
          parseFunctionAssignment(functionName, arguments, expressionStr, parser)
      }

  private def parseFunctionAssignment(functionName: String, arguments: String, expressionStr: String, parser: Parser): Either[Error, FunctionAssignment] =
    if !Dictionary.isValidName(functionName) then
      Left(ParsingError(s"Invalid function name: $functionName"))
    else if parser.dictionary.contains(functionName) then
      Left(ParsingError(s"The function already exists: $functionName"))
    else
      val argNames = arguments.split(",").map(_.trim).filter(_.nonEmpty).toSeq
      val errors = argNames.collect { case argName if !Dictionary.isValidName(argName) => argName }
      if errors.nonEmpty then
        Left(ParsingError(s"""Invalid argument(s): ${errors.mkString(", ")}"""))
      else
        val argsMap = argNames.map(name => name -> Variable(name)).toMap
        val innerDict = parser.dictionary.copy(argsMap)
        Parser(innerDict)
          .parse(expressionStr)
          .map {
            case Left(error) =>
              Left(error)
            case Right(expression) =>
              FunctionAssignment(functionName, argNames, expression).pipe { assignment =>
                parser.dictionary.add(functionName, assignment)
                Right(assignment)
              }
          }
          .getOrElse(Left(ParsingError(s"Unable to parse: $expressionStr")))
