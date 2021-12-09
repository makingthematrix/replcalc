package replcalc.eval

import Error.*

final case class Assignment(name: String, expr: Expression) extends Expression:
  override def evaluate: Either[Error, Double] = expr.evaluate

object Assignment extends Parseable[Assignment]:
  override def parse(line: String, dict: Dictionary): ParsedExpr[Assignment] =
    if !line.contains("=") then
      None
    else
      val assignIndex = line.indexOf('=')
      val name = line.substring(0, assignIndex).trim
      val exprStr = line.substring(assignIndex + 1).trim
      if !Value.isValidValueName(name) then
        Some(Left(ParsingError(s"Invalid value name: $name")))
      else if dict.contains(name) then
        Some(Left(ParsingError(s"The value $name is already defined")))
      else
        Parser.parse(exprStr, dict) match
          case Some(Right(expression)) =>
            dict.add(name, expression)
            Some(Right(Assignment(name, expression)))
          case Some(Left(error)) =>
            Some(Left(error))
          case None =>
            Some(Left(ParsingError(s"Unable to parse: $exprStr")))
