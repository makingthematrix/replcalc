package replcalc

import munit.{ComparisonFailException, Location}

class ExpressionTest extends munit.FunSuite:
  implicit val location: Location = Location.empty

  private def createParser(dict: Dictionary = Dictionary()): Parser = Parser(dict)

  private def eval(str: String, expected: Double, delta: Double = 0.001)(implicit parser: Parser): Unit =
    parser.parse(str) match
      case None => 
        failComparison("Parsed as 'none'", str, expected)
      case Some(Left(error)) => 
        failComparison(s"Error: ${error.msg}", str, expected)
      case Some(Right(expr)) =>
        expr.evaluate(parser.dictionary) match
          case Right(result) => assertEqualsDouble(result, expected, delta)
          case Left(error)   => failComparison(s"Error: ${error.msg}", str, expected)

  private def parse(str: String)(implicit parser: Parser): Unit =
    parser.parse(str) match
      case None              => fail(s"Parsed as 'none': $str")
      case Some(Left(error)) => fail(s"Error: ${error.msg} at line $str")
      case Some(Right(expr)) =>

  private def shouldReturnParsingError(line: String)(implicit parser: Parser): Unit =
    parser.parse(line) match
      case None              => fail(s"Parsed as 'none': $line")
      case Some(Left(error)) =>
      case Some(Right(expr)) => fail(s"Parsed with success: $line -> $expr")

  test("Number") {
    implicit def parser: Parser = createParser()
    eval("4", 4.0)
    eval("4.12", 4.12)
    eval("0", 0.0, 0.00001)
    shouldReturnParsingError("blah")
  }

  test("Add") {
    implicit def parser: Parser = createParser()
    eval("1+1", 2.0)
    eval("1+2+3", 6.0)
  }

  test("Substract") {
    implicit def parser: Parser = createParser()
    eval("2-1", 1.0)
    eval("3-2-1", 0.0)
    eval("1-2", -1.0)
  }

  test("Add and Substract") {
    implicit def parser: Parser = createParser()
    eval("3+2-1", 4.0)
    eval("3-2+1", 2.0)
    eval("3+2-1+4", 8.0)
    eval("3-2+1-4", -2.0)
  }

  test("Multiply") {
    implicit def parser: Parser = createParser()
    eval("1*1", 1.0)
    eval("1*2*3", 6.0)
    eval("5.0*2.5", 12.5)
    eval("3.0*0", 0.0)
  }

  test("Divide") {
    implicit def parser: Parser = createParser()
    eval("2/1", 2.0)
    eval("3/2/2", 0.75)
    eval("1.0/2.0", 0.5)
    intercept[ComparisonFailException](eval("1/0", Double.NaN))
  }

  test("Multiply and Divide") {
    implicit def parser: Parser = createParser()
    eval("3*2/1", 6.0)
    eval("3/2*1", 1.5)
    eval("3*2/2*4", 12.0)
    eval("3/2*2/4", 0.75)
  }

  test("Add and Substract and Multiply and Divide") {
    implicit def parser: Parser = createParser()
    eval("1+3*2/1", 7.0)
    eval("3/2*1+1", 2.5)
    eval("0-3*2/2*4", -12.0)
    eval("3/2+2/4-3*0.5", 0.5)
  }

  test("Unary minus") {
    implicit def parser: Parser = createParser()
    eval("-3", -3.0)
    eval("5*-3", -15.0)
    eval("2+-3", -1.0)
    eval("2--3", 5.0)
    eval("3/2+2/-4-3*0.5", -0.5)
    shouldReturnParsingError("-")
  }

  test("Unary and binary minus") {
    implicit def parser: Parser = createParser()
    eval("3--3", 6.0)
    eval("3+-3", 0.0)
    eval("3- -3", 6.0)
    eval("3 - - 3", 6.0)
  }

  test("Value assignments") {
    implicit def parser: Parser = createParser()
    eval("a = 3", 3.0)
    eval("_a = 3", 3.0)
    eval("a_ = 3", 3.0)
    eval("a_b = 3", 3.0)
    eval("a1 = 3", 3.0)
    shouldReturnParsingError("1a = 3")
    eval("a = 3 + 4", 7.0)
    eval("a = b = 3", 3.0)
  }

  test("Forbidden to reassign") {
    implicit val parser: Parser = createParser() // val, not def, so the same parser will be used in all evaluations
    eval("a = 1", 1.0)
    shouldReturnParsingError(" a = 2")
  }

  test("Use an assigned expression in another expression") {
    implicit val parser: Parser = createParser() // val, not def, so the same parser will be used in all evaluations
    eval("a = 1", 1.0)
    eval("a + 1", 2.0)
  }

  test("Use more than one assigned expression in another expression") {
    implicit val parser: Parser = createParser() // val, not def, so the same parser will be used in all evaluations
    eval("a = 1", 1.0)
    eval("b = 2", 2.0)
    eval("c = a + b", 3.0)
  }

  test("Handle and error when the value is not assigned") {
    implicit val parser: Parser = createParser() // val, not def, so the same parser will be used in all evaluations
    eval("a = 1", 1.0)
    eval("b = 2", 2.0)
    shouldReturnParsingError("c = d + e")
  }

  test("Expressions with parentheses") {
    implicit def parser: Parser = createParser()
    eval("1 + (2 + 3) + 4", 10.0)
    eval("1 - (2 + 3) - 4", -8.0)
    eval("-(3*-2)", 6.0)
    eval("(2+3)*2", 10.0)
    eval("1 + ((2 * 3) - 4) / 5", 1.4)
  }

  test("Function assignments") {
    implicit val parser: Parser = createParser()
    parse("foo(x) = x + 1")
    parse("bar() = 2 + 1")
    parse("a = 3")
    parse("myFunc(x) = a + x + 2")
    shouldReturnParsingError("myFunc(x) = y")
  }

  test("Functions with one argument") {
    implicit val parser: Parser = createParser()
    parse("foo(x) = x + 1")
    eval("foo(1)", 2.0)
    eval("foo(1) + 1", 3.0)
    eval("foo(1+2)", 4.0)
    eval("foo(foo(1))", 3.0)
    eval("foo(foo(1)+foo(1))", 5.0)
    parse("bar(x) = x - 1")
    eval("foo(1)+bar(1)", 2.0)
    eval("1+foo(2*(1+bar(3)))+bar((foo(4)+5)/2)+2", 14.0)
  }

  test("Functions with many arguments") {
    implicit val parser: Parser = createParser()
    parse("foo(x, y) = x + y")
    eval("foo(1, 2)", 3.0)
    eval("foo(foo(1, 2), foo(3, 4))", 10.0)
    parse("bar(x, y, z) = x + y + z")
    eval("1 + bar(foo(2, 1), 3, 4 + foo(5, 1))", 17.0)
  }

  test("Function using previously defined values") {
    implicit val parser: Parser = createParser()
    parse("x = 1")
    parse("foo(y) = x + y")
    eval("foo(2)", 3.0)
  }

  test("Function arguments shadowing previously defined values") {
    implicit val parser: Parser = createParser()
    parse("x = 1")
    parse("foo(x) = x + 1")
    eval("foo(2)", 3.0)
    eval("x", 1.0)
  }