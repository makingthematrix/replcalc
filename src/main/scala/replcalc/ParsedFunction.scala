package replcalc

import replcalc.Dictionary.isValidName
import replcalc.expressions.Error
import replcalc.expressions.Error.ParsingError
import replcalc.Preprocessor.{findParens, splitByCommas}

/**
 * Parsed function
 *
 * A small utility class for help in parsing a function or a function assignment out of a text chunk.
 * If the text is of the form `name(arg1, arg2, ...)`, the parsed function will keep the name and the list of
 * arguments. I will also properly return a `None` if the text doesn't seem to be a function (i.e. it does not have
 * parentheses) and it will return an error if it seems to be a function, but it can't be parsed.
 *
 * It's possible to use this class both for a function assignment and a function used in an expression, given that
 * the arguments are already wrapped in parentheses, so we know which commas denote the arguments of the given function
 * and which belong to nested functions.
 *
 * @param name A valid function name
 * @param arguments A sequence of valid arguments names or expressions in the text form, depending on whether an assignment
 *                  or an expression was parsed. Might be empty.
 */

final case class ParsedFunction(name: String, arguments: Seq[String])

object ParsedFunction:
  enum LineSide:
    case Left
    case Right

  def parse(line: String, lineSide: LineSide): Option[Either[Error, ParsedFunction]] =
    findParens(line, functionParens = true).map {
      case Left(error) =>
        Left(error)
      case Right((_, closing)) if closing + 1 < line.length =>
        Left(ParsingError(s"Unrecognized chunk of a function expression: ${line.substring(closing + 1)}"))
      case Right((opening, closing)) =>
        val name = line.substring(0, opening)
        if !isValidName(name) then
          Left(ParsingError(s"Invalid function name: $name"))
        else
          val argStr    = line.substring(opening + 1, closing)
          val arguments = splitByCommas(argStr)
          val errors    = arguments.collect {
            case argName if argName.isEmpty                                    => "Empty argument name"
            case argName if lineSide == LineSide.Left && !isValidName(argName) => argName
          }
          if errors.nonEmpty then
            Left(ParsingError(s"Invalid argument(s): ${errors.mkString(", ")}"))
          else
            Right(ParsedFunction(name, arguments))
    }
