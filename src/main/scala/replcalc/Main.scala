package replcalc

import replcalc.eval.{Parser, Dictionary}
import scala.io.StdIn.readLine

@main
def main(args: String*): Unit =
  val dict = Dictionary()
  var exit = false
  while !exit do
    print("> ")
    val line = readLine()
    val trimmed = line.trim
    if trimmed == ":exit" then 
      exit = true
    else if trimmed == ":list" then 
      listValues(dict)
    else 
      parseLine(trimmed, dict)

private def listValues(dict: Dictionary): Unit =
  dict.listNames.toSeq.sorted.foreach { name =>
    dict.get(name).map(_.evaluate).foreach {
      case Right(result) => println(s"$name -> $result")
      case Left(error)   => println(s"Error when evaluating $name: ${error.msg}")
    }
  }
  
private def parseLine(line: String, dict: Dictionary): Unit =
  Parser.parse(line, dict).map(_.flatMap(_.evaluate)) match
    case Some(Right(result)) => println(result)
    case Some(Left(error))   => println(s"Error: ${error.msg}")
    case None                => println(s"Error: Unable to parse the expression: $line")