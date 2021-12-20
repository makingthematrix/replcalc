package replcalc

import replcalc.Dictionary.isValidName
import replcalc.expressions.Error
import replcalc.expressions.Error.ParsingError
import replcalc.Preprocessor.{findParens, splitByCommas}

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

