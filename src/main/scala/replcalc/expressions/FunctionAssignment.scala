package replcalc.expressions

import Error.*
import replcalc.{Dictionary, Parser}

final case class FunctionAssignment(name: String, args: Set[String], expr: Expression) extends Expression:
  override def evaluate(dict: Dictionary): Either[Error, Double] = expr.evaluate(dict)

object FunctionAssignment extends Parseable[FunctionAssignment]:
  private def argsAsMap(args: Set[String]): Map[String, Expression] = args.map(arg => arg -> Value(arg)).toMap

  override def parse(line: String, parser: Parser): ParsedExpr[FunctionAssignment] =
    if !line.contains("=") then
      None
    else
      val assignIndex = line.indexOf('=')
      val name = line.substring(0, assignIndex)
      if name.count(_ == '(') != 1 || name.count(_ == ')') != 1 then
        None
      else
        parseFunction(name, line.substring(assignIndex + 1), parser)

  private def parseFunction(name: String, exprStr: String, parser: Parser): ParsedExpr[FunctionAssignment] =
    val startIndex = name.indexOf('(')
    val endIndex = name.indexOf(')')
    val functionName = name.substring(0, startIndex)
    if startIndex > endIndex || !Value.isValidValueName(functionName) then
      Some(Left(ParsingError(s"Invalid function name: $functionName")))
    else
      val args = name.substring(startIndex + 1, endIndex).split(",").map(_.trim).filter(_.nonEmpty).toSet
      args.collect { case arg if !Value.isValidValueName(arg) => arg } match
        case invalidArgs if invalidArgs.nonEmpty =>
          Some(Left(ParsingError(s"Invalid argument(s): ${invalidArgs.mkString(", ")}")))
        case _ =>
          val innerDict = parser.dictionary.copy(argsAsMap(args))
          Parser(innerDict).parse(exprStr) match
            case Some(Right(expression)) =>
              val assignment = FunctionAssignment(functionName, args, expression)
              parser.dictionary.add(functionName, assignment)
              Some(Right(assignment))
            case Some(Left(error)) =>
              Some(Left(error))
            case None =>
              Some(Left(ParsingError(s"Unable to parse: $exprStr")))

