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
      val left = line.substring(0, assignIndex)
      Preprocessor.findParens(left, functionParens = true).flatMap {
        case Left(error) =>
          Some(Left(error))
        case Right((opening, closing)) =>
          val functionName = left.substring(0, opening)
          val argsStr = left.substring(opening + 1, closing)
          val right = line.substring(assignIndex + 1)
          parseFunction(functionName, argsStr, right, parser)
      }

  private def parseFunction(functionName: String, argsStr: String, exprStr: String, parser: Parser): ParsedExpr[FunctionAssignment] =
    if !Dictionary.isValidName(functionName) then
      Some(Left(ParsingError(s"Invalid function name: $functionName")))
    else
      val argNames = argsStr.split(",").map(_.trim).filter(_.nonEmpty).toSeq
      val error = argNames.collect { case argName if !Dictionary.isValidName(argName) => argName }
      error match
        case invalidArgs if invalidArgs.nonEmpty =>
          Some(Left(ParsingError(s"Invalid argument(s): ${invalidArgs.mkString(", ")}")))
        case _ =>
          val argsMap = argNames.map(name => name -> Value(name)).toMap
          val innerDict = parser.dictionary.copy(argsMap)
          Parser(innerDict).parse(exprStr) match
            case Some(Right(expression)) =>
              FunctionAssignment(functionName, argNames, expression).pipe { assignment =>
                parser.dictionary.add(functionName, assignment)
                Some(Right(assignment))
              }
            case Some(Left(error)) =>
              Some(Left(error))
            case None =>
              Some(Left(ParsingError(s"Unable to parse: $exprStr")))
