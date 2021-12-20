package replcalc.expressions

import replcalc.Preprocessor.ParsedFunction
import replcalc.{Dictionary, Parser, Preprocessor}
import replcalc.Preprocessor.LineSide
import replcalc.expressions.Error.{EvaluationError, ParsingError}

final case class Function(name: String, args: Seq[Expression]) extends Expression:
  override protected def evaluate(dict: Dictionary): Either[Error, Double] =
    dict.get(name) match
      case Some(f: FunctionAssignment) if f.argNames.length == args.length =>
        val argMap = f.argNames.zip(args).toMap
        f.run(dict.copy(argMap))
      case _ =>
        Left(EvaluationError(s"Function not found: $name with ${args.length} arguments"))

object Function extends Parseable[Function]:
  override def parse(parser: Parser, line: String): ParsedExpr[Function] =
    Preprocessor.parseFunction(line, LineSide.Right).flatMap {
      case Left(error) =>
        ParsedExpr.error(error)
      case Right(ParsedFunction(name, _)) if !parser.dictionary.contains(name) =>
        ParsedExpr.error(s"Function not found: $name")
      case Right(ParsedFunction(name, arguments)) =>
        parseFunction(parser, name, arguments)
    }

  private def parseFunction(parser: Parser, name: String, args: Seq[String]): ParsedExpr[Function] =
    val parsedArgs = args.map { arg => arg -> parser.parse(arg) }
    val errors = parsedArgs.collect {
      case (argName, None)              => s"Unable to parse argument $argName"
      case (argName, Some(Left(error))) => s"Unable to parse argument $argName: ${error.msg}"
    }
    if errors.nonEmpty then
      ParsedExpr.error(errors.mkString("; "))
    else
      val validArgs = parsedArgs.collect { case (_, Some(Right(expr))) => expr }
      ParsedExpr(Function(name, validArgs))
