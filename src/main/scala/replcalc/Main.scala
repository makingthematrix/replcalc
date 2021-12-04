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
      val result = Expression(text).evaluate
      println(result)
