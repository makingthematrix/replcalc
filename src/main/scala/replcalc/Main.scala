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
  dict.listNames().toSeq.sorted.map(name => name -> dict.get(name)).map {
    case (name, Some(FunctionAssignment(_, args, _))) =>
      s"$name(${args.mkString(",")}) -> Function"
    case (name, Some(expr)) =>
      expr.evaluate(dict) match
        case Right(result) =>
          s"$name -> $result"
        case Left(error)   =>
          s"Error when evaluating $name: ${error.msg}"
    case (name, _) => s"Error: Unable to find an expression for $name"
  }.foreach(println)
  
private def evaluate(line: String, parser: Parser): Option[String] =
  parser.parse(line).map {
    case Right(FunctionAssignment(name, args, _)) =>
      s"$name(${args.mkString(",")}) -> Function"
    case Right(ValueAssignment(name, expr)) =>
      expr.evaluate(parser.dictionary) match
        case Right(result) => s"$name -> $result"
        case Left(error)   => s"$name -> Evaluation error: ${error.msg}"
    case Right(expr) =>
      expr.evaluate(parser.dictionary) match
        case Right(result) => s"$result"
        case Left(error)   => s"Evaluation error: ${error.msg}"
    case Left(error) =>
      s"Parsing error: ${error.msg}"
  }
