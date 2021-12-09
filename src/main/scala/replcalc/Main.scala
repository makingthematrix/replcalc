package replcalc

import replcalc.eval.{Parser, Dictionary}
import scala.io.StdIn.readLine

@main
def main(args: String*): Unit =
  val dictionary = Dictionary()
  var exit = false
  while !exit do
    print("> ")
    val line = readLine()
    if line.trim == ":exit" then
      exit = true
    else
      Parser.parse(line, dictionary).map(_.flatMap(_.evaluate)) match
        case Some(Right(result)) => println(result)
        case Some(Left(error))   => println(s"Error: ${error.msg}")
        case None                => println(s"Error: Unable to parse the expression: $line")
