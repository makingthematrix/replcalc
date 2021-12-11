package replcalc

import replcalc.expressions.{Expression, FunctionAssignment, ValueAssignment}
import scala.io.StdIn.readLine

@main
def main(args: String*): Unit =
  lazy val parser = Parser()

  var exit = false
  while !exit do
    print("> ")
    val line = readLine()
    val trimmed = line.trim
    if trimmed == ":exit" then 
      exit = true
    else if trimmed == ":list" then 
      listValues(parser.dictionary)
    else
      evaluate(trimmed, parser).foreach(println)

private def listValues(dict: Dictionary): Unit =
  dict
    .listNames().toSeq.sorted
    .map(name => dict.get(name))
    .collect { case Some(expr) => replForm(expr, dict) }
    .foreach(println)
  
private def evaluate(line: String, parser: Parser): Option[String] =
  parser.parse(line).map {
    case Right(expr) => replForm(expr, parser.dictionary)
    case Left(error) => s"Parsing error: ${error.msg}"
  }

private def replForm(expr: Expression, dict: Dictionary): String =
  expr match
    case FunctionAssignment(name, args, _) => s"$name(${args.mkString(",")}) -> Function"
    case ValueAssignment(name, expr) =>
      expr.evaluate(dict) match
        case Right(result) => s"$name -> $result"
        case Left(error)   => s"$name -> Evaluation error: ${error.msg}"
    case expr =>
      expr.evaluate(dict) match
        case Right(result) => result.toString
        case Left(error)   => s"Evaluation error: ${error.msg}"