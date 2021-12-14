package replcalc

import replcalc.Preprocessor.Flags
import replcalc.expressions.Error

import scala.annotation.tailrec

final class Preprocessor(parser: Parser, flags: Flags = Flags.AllTrue):
  import Preprocessor.*

  def process(line: String): Either[Error, String] =
    for
      line          <- if flags.removeWhitespaces then removeWhitespaces(line) else Right(line)
      assignIndex   =  line.indexOf('=')
      (left, right) =  if assignIndex > 0 then (line.substring(0, assignIndex), line.substring(assignIndex + 1)) else ("", line)
      right         <- if flags.wrapFunctionArguments then wrapFunctionArguments(right) else Right(right)
      right         <- if flags.removeParens then removeParens(right) else Right(right)
    yield
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
    withParens(line, functionParens = false) { (opening, closing) =>
      parser.parse(line.substring(opening + 1, closing)) match
        case None =>
          Left(Error.PreprocessorError(s"Unable to parse: $line"))
        case Some(Left(error)) =>
          Left(error)
        case Some(Right(expr)) =>
          val pre = line.substring(0, opening)
          val name = parser.dictionary.addSpecial(expr)
          val post = line.substring(closing + 1)
          removeParens(s"$pre$name$post")
    }

  private def wrapFunctionArguments(line: String): Either[Error, String] =
    withParens(line, functionParens = true) { (opening, closing) =>
      val inside = line.substring(opening + 1, closing)
      val args =
        if inside.isEmpty then
          Nil
        else
          splitByCommas(inside).map {
            case arg if arg.forall(c => !Parser.isOperator(c)) && findParens(arg, functionParens = true).isEmpty =>
              Right(arg)
            case arg if arg.head == '(' && arg.last == ')' && arg.count(_ == '(') == 1 && arg.count(_ == ')') == 1 =>
              Right(arg)
            case arg =>
              wrapFunctionArguments(arg).map(wrapped => s"($wrapped)")
          }
      args.find(_.isLeft) match
        case Some(error) =>
          error
        case None =>
          wrapFunctionArguments(line.substring(closing + 1)).map { post =>
            val pre = line.substring(0, opening)
            val wrapped = args.collect { case Right(arg) => arg }.mkString(",")
            s"$pre($wrapped)$post"
          }
    }

  private def withParens(line: String, functionParens: Boolean)(body: (Int, Int) => Either[Error, String]): Either[Error, String] =
    findParens(line, functionParens) match
      case None =>
        Right(line)
      case Some(Left(error)) =>
        Left(error)
      case Some(Right(opening, closing)) =>
        body(opening, closing)

object Preprocessor:
  final case class Flags(removeWhitespaces: Boolean = true,
                         wrapFunctionArguments: Boolean = true,
                         removeParens: Boolean = true)

  object Flags:
    val AllTrue: Flags = Flags()

  def findParens(line: String, functionParens: Boolean): Option[Either[Error, (Int, Int)]] =
    inline def isFunctionParens(line: String, atIndex: Int): Boolean = atIndex != 0 && !Parser.isOperator(line(atIndex - 1))
    val opening = line.indexOf('(')
    if opening == -1 then
      None
    else if (functionParens && isFunctionParens(line, opening)) ||
      (!functionParens && !isFunctionParens(line, opening)) then
      findClosingParens(line.substring(opening)) match
        case None =>
          Some(Left(Error.PreprocessorError(s"Unable to find the matching closing parenthesis: $line")))
        case Some(offset) =>
          Some(Right((opening, opening + offset)))
    else
      findParens(line.substring(opening + 1), functionParens)
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

  def splitByCommas(line: String): List[String] =
    findNextComma(line) match
      case None             => List(line)
      case Some(commaIndex) => line.substring(0, commaIndex) :: splitByCommas(line.substring(commaIndex + 1))

  private def findNextComma(line: String): Option[Int] =
    val commaIndex = line.indexOf(',')
    if commaIndex == -1 then
      None
    else if commaIndex == 0 then
      Some(0)
    else
      findParens(line, true) match
        case None                                              => Some(commaIndex)
        case Some(Left(error))                                 => None
        case Some(Right((opening, _))) if commaIndex < opening => Some(commaIndex)
        case Some(Right((_, closing))) if commaIndex > closing => Some(commaIndex)
        case Some(Right((_, closing)))                         => findNextComma(line.substring(closing + 1)).map(_ + closing + 1)
