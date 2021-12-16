package replcalc.expressions

import Error.*
import replcalc.{Dictionary, Parser}
import scala.util.chaining.*

final case class Assignment(name: String, constant: Constant) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = Right(constant.number)

object Assignment extends Parseable[Assignment]:
  override def parse(line: String, parser: Parser): ParsedExpr[Assignment] =
    if !line.contains("=") then
      ParsedExpr.empty
    else
      val assignIndex = line.indexOf('=')
      parseAssignment(line.substring(0, assignIndex), line.substring(assignIndex + 1), parser)

  private def parseAssignment(name: String, expressionStr: String, parser: Parser): ParsedExpr[Assignment] =
    if !Dictionary.isValidName(name) then
      ParsedExpr.error(s"Invalid variable name: $name")
    else if !parser.dictionary.canAssign(name) then
      ParsedExpr.error(s"Unable to assign to: $name")
    else
      parser
        .parse(expressionStr)
        .happyPath { expression =>
          expression
            .evaluate(parser.dictionary).map { number =>
              Assignment(name, Constant(number)).tap { parser.dictionary.add(name, _) }
            }
            .pipe(Some(_))
        }
        .errorIfEmpty(s"Unable to parse: $expressionStr")
