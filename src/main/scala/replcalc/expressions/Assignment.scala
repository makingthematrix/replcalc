package replcalc.expressions

import Error.*
import replcalc.{Dictionary, Parser}

final case class Assignment(name: String, expr: Expression) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = expr.evaluate(dict)

object Assignment extends Parseable[Assignment]:
  override def parse(line: String, parser: Parser): ParsedExpr[Assignment] =
    if !line.contains("=") then
      None
    else
      val assignIndex = line.indexOf('=')
      val name = line.substring(0, assignIndex)
      if !Value.isValidValueName(name) then
        Some(Left(ParsingError(s"Invalid value name: $name")))
      else if parser.containsValue(name) then
        Some(Left(ParsingError(s"The value $name is already defined")))
      else
        val exprStr = line.substring(assignIndex + 1)
        parser.parse(exprStr) match
          case Some(Right(expression)) =>
            parser.addValue(name, expression)
            Some(Right(Assignment(name, expression)))
          case Some(Left(error)) =>
            Some(Left(error))
          case None =>
            Some(Left(ParsingError(s"Unable to parse: $exprStr")))
