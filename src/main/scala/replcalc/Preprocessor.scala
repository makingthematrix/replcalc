package replcalc

import replcalc.expressions.Error.ParsingError

import scala.annotation.tailrec

final class Preprocessor(parser: Parser):
  private var specialValuesCounter = 0L

  def process(line: String): Either[ParsingError, String] =
    for {
      noWhitespaces <- removeWhitespaces(line)
      noParentheses <- removeParentheses(noWhitespaces)
    } yield noParentheses

  private def removeWhitespaces(line: String): Either[ParsingError, String] =
    if line.forall(!_.isWhitespace) then
      Right(line)
    else
      val sb = StringBuilder()
      line.foreach {
        case ch if !ch.isWhitespace => sb.addOne(ch)
        case _ =>
      }
      Right(sb.toString)

  @tailrec
  private def removeParentheses(line: String): Either[ParsingError, String] =
    val opening = line.indexOf("(")
    if opening == -1 then
      Right(line)
    else
      findMatchingParens(line.substring(opening)) match
        case Some(index) =>
          val closing = opening + index
          parser.parse(line.substring(opening + 1, closing)) match
            case Some(Right(expr)) =>
              specialValuesCounter += 1
              val name = s"$$$specialValuesCounter"
              parser.addValue(name, expr)
              removeParentheses(s"${line.substring(0, opening)}$name${line.substring(closing + 1)}")
            case Some(Left(error)) =>
              Left(error)
            case None =>
              Left(ParsingError(s"Preprocessor: Unable to parse: $line"))
        case None =>
          Left(ParsingError(s"Preprocessor: Unable to find the matching closing parenthesis: $line"))

  private def findMatchingParens(expr: String): Option[Int] =
    if expr.isEmpty then None
    else
      val (index, counter) = expr.drop(1).foldLeft((0, 1)) {
        case ((index, 0), _)                         => (index, 0)
        case ((index, counter), '(')                 => (index + 1, counter + 1)
        case ((index, counter), ')') if counter == 0 => (index, counter - 1)
        case ((index, counter), ')')                 => (index + 1, counter - 1)
        case ((index, counter), _)                   => (index + 1, counter)
      }
      if counter == 0 then Some(index) else None
