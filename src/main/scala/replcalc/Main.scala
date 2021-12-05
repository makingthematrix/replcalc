package replcalc

import replcalc.eval.Expression

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
      Expression.parse(text).flatMap(_.evaluate) match {
        case Some(result) => println(result)
        case None         => println("Error: Unable to parse or evaluate")
      }
