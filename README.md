[![Scala CI](https://github.com/makingthematrix/replcalc/actions/workflows/scala.yml/badge.svg)](https://github.com/makingthematrix/replcalc/actions/workflows/scala.yml)

# REPLcalc

This is a small REPL calculator written in Scala 3.1.
I started it using some code from the calculator from my [Scala on Android](https://github.com/makingthematrix/scalaonandroid) project,
but since then it has grown a lot. Actually, now I should replace that code with this.

### How to use it

You need [`sbt`](https://www.scala-sbt.org/index.html). 
When you have it installed, download this project, go to its main folder with a terminal of your choice and type `sbt run`.
After `sbt` download and compiles everything it needs, you should see something like this:
```
[info] running replcalc.main 
> 
```
That `>` is a prompt. Now you can write mathematical expressions and the program will display their results.
If you want to exit the program, type `:exit`. (Note the `:` sign).

### What does it support

* Basic mathematical operations: addition, substraction, multiplication, division, and negation (i.e. a unary '-' operator).
* Parentheses.
* Variables. Use `<a variable name> = <any expression>` to define a variable. From now you can use it in your next expressions.
* Reassignments. A previously defined variable can be reassigned to a new value.
* Functions. Use `functionName(arg1, arg2, ...) = <any expression>` to define a function. The expression on the right side of `=` can use the arguments as well as previously defined variables and functions. Note that a function result is calculated when the function is called, so if in the mean time you reassign a variable used in the function, the next call to the function may give you a different result.
* A function defined with an empty list of arguments can be later used in expressions both with parentheses and without. E.g. `foo() = 1` can be called both as `foo()` and `foo`. 
* You can see all functions and variables you defined by typing `:list`.
* If you make a typo or another error, the calculator will try it best to display a meaningful error message.

### What does it not support

* No function reassignment.
* No function overloading. There can be only one function of a given name. Even if you want to create a function with the same name but a different number of arguments, it won't work.
* No default arguments in functions.
* No recursion. A function must be first defined to be used in an expression, so e.g. `foo(x) = foo(x + 1)` won't work. However, `x = x + 1` will work, given that `x` was defined previously, and it is simply being reassigned.
* No control flow syntax. Sorry. This is just a calculator.

### Other notes

Apart from playing with the calculator, you can look through the history of pull requests, the tickets on the GitHub Projects page, and also through comments in the headers of each file, and read all about the process I used and what was I thinking while working on it. You will see how I introduced each feature, but also how I made some changes that were later removed, how I found and fixed bugs, etc. 
