package replcalc.eval

import Error.*

final case class Assignment(name: String, expression: Expression) extends Expression:
  override def evaluate: Either[Error, Double] = expression.evaluate

object Assignment extends Parseable[Assignment]:
  override def parse(line: String): ParsedExpr[Assignment] =
    if !line.contains("=") then
      None
    else
      val assignIndex = line.indexOf('=')
      val name = line.substring(0, assignIndex).trim
      val exprStr = line.substring(assignIndex + 1).trim
      if !isValidName(name) then
        Some(Left(ParsingError(s"Invalid value name: $name")))
      else
        Expression.parse(exprStr) match
          case Some(Right(expression)) => Some(Right(Assignment(name, expression)))
          case Some(Left(error))       => Some(Left(error))
          case None                    => Some(Left(ParsingError(s"Unable to parse: $exprStr")))

  private def isValidName(name: String): Boolean =
    name.nonEmpty &&
      (name(0).isLetter || name(0) == '_') &&
      (name.substring(1).forall(ch => ch.isLetterOrDigit || ch == '_'))