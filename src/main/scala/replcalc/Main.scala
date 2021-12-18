package replcalc

import replcalc.expressions.{Constant, Expression, FunctionAssignment, Assignment}
import scala.util.chaining.*
import scala.io.StdIn.readLine

@main
def main(args: String*): Unit =
  val parser = Parser()
  var exit = false
  while !exit do
    print("> ")
    readLine().trim match
      case ":exit"              => exit = true
      case ":list"              => list(parser.dictionary)
      case line if line.isEmpty =>
      case line                 => run(parser, line).foreach(println)

private def list(dictionary: Dictionary): Unit =
  dictionary
    .expressions
    .toSeq.sortBy(_._1).map(_._2)
    .map(replForm(dictionary, _))
    .foreach(println)
  
private def run(parser: Parser, line: String): Option[String] =
  parser.parse(line).map {
    case Right(expr) =>
      replForm(parser.dictionary, expr).tap { _ => parser.dictionary.cleanSpecials() }
    case Left(error) =>
      s"Parsing error: ${error.msg}"
  }

private def replForm(dictionary: Dictionary, expression: Expression): String =
  expression match
    case FunctionAssignment(name, args, _) =>
      s"$name(${args.mkString(", ")}) -> Function"
    case Assignment(name, Constant(number)) =>
      s"$name -> $number"
    case expr =>
      expr.run(dictionary) match
        case Right(result) => result.toString
        case Left(error)   => s"Evaluation error: ${error.msg}"