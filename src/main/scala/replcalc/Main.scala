package replcalc

import replcalc.eval.{Error, Expression}
import scala.io.StdIn.readLine

@main
def main(args: String*): Unit =
  var exit = false
  while !exit do
    print("> ")
    val text = readLine()
    if text.trim == ":exit" then
      exit = true
    else
      Expression.parse(text).map(_.flatMap(_.evaluate)) match
        case Some(Right(result)) => println(result)
        case Some(Left(error))   => println(s"Error: ${error.msg}")
        case None                => println(s"Error: Unable to parse the expression: $text")
