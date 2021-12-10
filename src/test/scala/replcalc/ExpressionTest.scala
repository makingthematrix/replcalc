package replcalc

import munit.{ComparisonFailException, Location}

class ExpressionTest extends munit.FunSuite:
  implicit val location: Location = Location.empty

  private def createParser(pre: Preprocessor = Preprocessor(), dict: Dictionary = Dictionary()): Parser = Parser(pre, dict)

  private def eval(str: String, expected: Double, delta: Double = 0.001)(implicit parser: Parser) =
    parser.parse(str) match
      case None => failComparison("Parsed as 'none'", str, expected)
      case Some(Left(error)) => failComparison(s"Error: ${error.msg}", str, expected)
      case Some(Right(expr)) =>
        expr.evaluate match
          case Right(result) => assertEqualsDouble(result, expected, delta)
          case Left(error)   => failComparison(s"Error: ${error.msg}", str, expected)

  private def shouldReturnParsingError(line: String)(implicit parser: Parser) =
    parser.parse(line) match
      case None => fail(s"Parsed as 'none': $line")
      case Some(Right(expr)) => fail(s"Parsed with success: $line -> $expr")
      case Some(Left(error)) =>

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

  test("Assignments") {
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
