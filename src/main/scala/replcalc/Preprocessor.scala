package replcalc

import replcalc.Preprocessor.Flags
import replcalc.expressions.Error

import scala.annotation.tailrec

final class Preprocessor(parser: Parser, flags: Flags = Flags.AllTrue):
  def process(line: String): Either[Error, String] =
    for {
      line          <- if flags.removeWhitespaces then removeWhitespaces(line) else Right(line)
      assignIndex   =  line.indexOf('=')
      (left, right) =  if assignIndex == -1 then
                         ("", line)
                       else
                         (line.substring(0, assignIndex), line.substring(assignIndex + 1))
      right         <- if flags.removeParens then removeParens(right) else Right(right)
    } yield
      if left.isEmpty then right else s"$left=$right"

  private def removeWhitespaces(line: String): Either[Error, String] =
    if line.forall(!_.isWhitespace) then
      Right(line)
    else
      val sb = StringBuilder(line.length)
      line.foreach {
        case ch if !ch.isWhitespace => sb.addOne(ch)
        case _ =>
      }
      Right(sb.toString)

  private def removeParens(line: String): Either[Error, String] =
    withParens(line) { (opening, closing) =>
      parser.parse(line.substring(opening + 1, closing)) match
        case None =>
          Left(Error.ParsingError(s"Unable to parse: $line"))
        case Some(Left(error)) =>
          Left(error)
        case Some(Right(expr)) =>
          val pre = line.substring(0, opening)
          val name = parser.dictionary.addSpecial(expr)
          val post = line.substring(closing + 1)
          removeParens(s"$pre$name$post")
    }

  private def withParens(line: String)(body: (Int, Int) => Either[Error, String]): Either[Error, String] =
    findParens(line) match
      case None =>
        Right(line)
      case Some(Left(error)) =>
        Left(error)
      case Some(Right(opening, closing)) =>
        body(opening, closing)

  private def findParens(line: String): Option[Either[Error, (Int, Int)]] =
    val opening = line.indexOf('(')
    if opening == -1 then
      None
    else if opening == 0 || Parser.isOperator(line(opening - 1)) then
      findClosingParens(line.substring(opening)) match
        case None =>
          Some(Left(Error.ParsingError(s"Unable to find the matching closing parenthesis: $line")))
        case Some(offset) =>
          Some(Right((opening, opening + offset)))
    else
      findParens(line.substring(opening + 1))
        .map(res => res.map { case (op, cl) => (opening + 1 + op, opening + 1 + cl) })

  private def findClosingParens(expr: String): Option[Int] =
    if expr.isEmpty then
      None
    else
      val (index, counter) = expr.drop(1).foldLeft((0, 1)) {
        case ((index, 0), _)                         => (index, 0)
        case ((index, counter), '(')                 => (index + 1, counter + 1)
        case ((index, counter), ')') if counter == 0 => (index, counter - 1)
        case ((index, counter), ')')                 => (index + 1, counter - 1)
        case ((index, counter), _)                   => (index + 1, counter)
      }
      if counter == 0 then Some(index) else None

object Preprocessor:
  final case class Flags(removeWhitespaces: Boolean = true,
                         removeParens: Boolean = true)

  object Flags:
    val AllTrue: Flags = Flags()
