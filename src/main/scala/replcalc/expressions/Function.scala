package replcalc.expressions

import replcalc.{Dictionary, Parser, Preprocessor}
import replcalc.expressions.Error.{EvaluationError, ParsingError}

final case class Function(name: String, args: Seq[Expression]) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] =
    dict.get(name) match
      case Some(f: FunctionAssignment) if f.argNames.length == args.length =>
        val argMap = f.argNames.zip(args).toMap
        f.evaluate(dict.copy(argMap))
      case _ =>
        Left(EvaluationError(s"Function not found: $name"))

object Function extends Parseable[Function]:
  override def parse(line: String, parser: Parser): ParsedExpr[Function] =
    Preprocessor.findParens(line, functionParens = true) match
      case None =>
        None
      case Some(Left(error)) =>
        Some(Left(error))
      case Some(Right((opening, closing))) =>
        val name = line.substring(0, opening)
        if !Dictionary.isValidName(name) then
          None
        else if !parser.dictionary.contains(name) then
          Some(Left(ParsingError(s"Function not found: $name")))
        else if closing + 1 < line.length then
          Some(Left(ParsingError(s"Unrecognized chunk of a function expression: ${line.substring(closing + 1)}")))
        else
          val args =
            line
              .substring(opening + 1, closing)
              .split(",")
              .map(arg => arg -> parser.parse(arg))
              .toSeq
          val errors = args.collect {
            case (argName, None)              => s"Unable to parse argument: $argName"
            case (argName, Some(Left(error))) => s"Error while evaluating argument $argName: ${error.msg}"
          }
          if errors.nonEmpty then
            Some(Left(ParsingError(s"""${errors.mkString("; ")}""")))
          else
            val validArgs: Seq[Expression] = args.collect { case (_, Some(Right(expr))) => expr }
            Some(Right(Function(name, validArgs)))