package replcalc.expressions

import Error.*
import replcalc.{Dictionary, Parser}

final case class ValueAssignment(name: String, expr: Expression) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = expr.evaluate(dict)

object ValueAssignment extends Parseable[ValueAssignment]:
  override def parse(line: String, parser: Parser): ParsedExpr[ValueAssignment] =
    if !line.contains("=") then
      None
    else
      val assignIndex = line.indexOf('=')
      parseValue(line.substring(0, assignIndex), line.substring(assignIndex + 1), parser)

  private def parseValue(name: String, exprStr: String, parser: Parser): ParsedExpr[ValueAssignment] =
    if !Dictionary.isValidName(name) then
      Some(Left(ParsingError(s"Invalid value name: $name")))
    else if parser.dictionary.contains(name) then
      Some(Left(ParsingError(s"The value $name is already defined")))
    else
      parser.parse(exprStr) match
        case Some(Right(expression)) =>
          val assignment = ValueAssignment(name, expression)
          parser.dictionary.add(name, assignment)
          Some(Right(assignment))
        case Some(Left(error)) =>
          Some(Left(error))
        case None =>
          Some(Left(ParsingError(s"Unable to parse: $exprStr")))
