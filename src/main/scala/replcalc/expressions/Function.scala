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
  override def parse(parser: Parser, line: String): ParsedExpr[Function] =
    Preprocessor.findParens(line, functionParens = true).flatMap {
      case Left(error) =>
        ParsedExpr.error(error)
      case Right((opening, closing)) =>
        val name = line.substring(0, opening)
        if !Dictionary.isValidName(name) then
          ParsedExpr.empty
        else if !parser.dictionary.contains(name) then
          ParsedExpr.error(s"Function not found: $name")
        else if closing + 1 < line.length then
          ParsedExpr.error(s"Unrecognized chunk of a function expression: ${line.substring(closing + 1)}")
        else
          val args =
            line
              .substring(opening + 1, closing)
              .split(",")
              .collect { case arg if arg.nonEmpty => arg -> parser.parse(arg) }
              .toSeq
          val errors = args.collect {
            case (argName, None)              => s"Unable to parse argument: $argName"
            case (argName, Some(Left(error))) => s"Error while evaluating argument $argName: ${error.msg}"
          }
          if errors.nonEmpty then
            ParsedExpr.error(errors.mkString("; "))
          else
            val validArgs = args.collect { case (_, Some(Right(expr))) => expr }
            ParsedExpr(Function(name, validArgs))
    }
