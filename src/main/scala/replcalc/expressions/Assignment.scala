package replcalc.expressions

import Error.*
import replcalc.{Dictionary, Parser}
import scala.util.chaining.*

final case class Assignment(name: String, constant: Constant) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = Right(constant.number)

object Assignment extends Parseable[Assignment]:
  override def parse(line: String, parser: Parser): ParsedExpr[Assignment] =
    if !line.contains("=") then
      None
    else
      val assignIndex = line.indexOf('=')
      Some { parseAssignment(line.substring(0, assignIndex), line.substring(assignIndex + 1), parser) }

  private def parseAssignment(name: String, expressionStr: String, parser: Parser): Either[Error, Assignment] =
    if !Dictionary.isValidName(name) then
      Left(ParsingError(s"Invalid variable name: $name"))
    else if !parser.dictionary.canAssign(name) then
      Left(ParsingError(s"Unable to assign to: $name"))
    else
      parser
        .parse(expressionStr)
        .map {
          case Left(error) =>
            Left(error)
          case Right(expression) =>
            expression.evaluate(parser.dictionary).map { number =>
              Assignment(name, Constant(number)).tap { parser.dictionary.add(name, _) }
            }
        }
        .getOrElse(Left(ParsingError(s"Unable to parse: $expressionStr")))
