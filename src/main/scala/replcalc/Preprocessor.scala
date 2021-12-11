package replcalc

import replcalc.expressions.Error.ParsingError

import scala.annotation.tailrec

final class Preprocessor(parser: Parser):
  def process(line: String): Either[ParsingError, String] =
    for {
      line          <- removeWhitespaces(line)
      assignIndex   =  line.indexOf('=')
      (left, right) =  if assignIndex == -1 then ("", line)
                       else (line.substring(0, assignIndex), line.substring(assignIndex + 1))
      right         <- removeParentheses(right)
    } yield
      if left.isEmpty then right else s"${left}=${right}"

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
    val opening = line.indexOf('(')
    if opening == -1 then
      Right(line)
    else
      findMatchingParens(line.substring(opening)) match
        case Some(index) =>
          val closing = opening + index
          parser.parse(line.substring(opening + 1, closing)) match
            case Some(Right(expr)) =>
              val pre = line.substring(0, opening)
              val name = parser.dictionary.addSpecial(expr)
              val post = line.substring(closing + 1)
              removeParentheses(s"$pre$name$post")
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
