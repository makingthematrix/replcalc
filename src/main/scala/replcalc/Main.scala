package replcalc

import replcalc.expressions.Assignment

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
  dict.listNames().toSeq.sorted.foreach { name =>
    dict.get(name).map(_.evaluate(dict)).foreach {
      case Right(result) => println(s"$name -> $result")
      case Left(error)   => println(s"Error when evaluating $name: ${error.msg}")
    }
  }
  
private def evaluate(line: String, parser: Parser): Option[String] =
  parser.parse(line).map {
    case Right(Assignment(name, expr)) =>
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
