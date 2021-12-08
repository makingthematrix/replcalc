package replcalc.eval

import munit.{ComparisonFailException, Location}

class ExpressionTest extends munit.FunSuite:
  implicit val location: Location = Location.empty

  private def eval(str: String, expected: Double, delta: Double = 0.001) =
    Parser.parse(str) match
      case None => failComparison("Unable to parse", str, expected)
      case Some(Left(error)) => failComparison(s"Error: ${error.msg}", str, expected)
      case Some(Right(expr)) =>
        expr.evaluate match
          case Right(result) => assertEqualsDouble(result, expected, delta)
          case Left(error)   => failComparison(s"Error: ${error.msg}", str, expected)

  test("Number") {
    eval("4", 4.0)
    eval("4.12", 4.12)
    eval("0", 0.0, 0.00001)
    intercept[ComparisonFailException](eval("blah", Double.NaN))
  }

  test("Add") {
    eval("1+1", 2.0)
    eval("1+2+3", 6.0)
  }

  test("Substract") {
    eval("2-1", 1.0)
    eval("3-2-1", 0.0)
    eval("1-2", -1.0)
  }

  test("Add and Substract") {
    eval("3+2-1", 4.0)
    eval("3-2+1", 2.0)
    eval("3+2-1+4", 8.0)
    eval("3-2+1-4", -2.0)
  }

  test("Multiply") {
    eval("1*1", 1.0)
    eval("1*2*3", 6.0)
    eval("5.0*2.5", 12.5)
    eval("3.0*0", 0.0)
  }

  test("Divide") {
    eval("2/1", 2.0)
    eval("3/2/2", 0.75)
    eval("1.0/2.0", 0.5)
    intercept[ComparisonFailException](eval("1/0", Double.NaN))
  }

  test("Multiply and Divide") {
    eval("3*2/1", 6.0)
    eval("3/2*1", 1.5)
    eval("3*2/2*4", 12.0)
    eval("3/2*2/4", 0.75)
  }

  test("Add and Substract and Multiply and Divide") {
    eval("1+3*2/1", 7.0)
    eval("3/2*1+1", 2.5)
    eval("0-3*2/2*4", -12.0)
    eval("3/2+2/4-3*0.5", 0.5)
  }

  test("Unary minus") {
    eval("-3", -3.0)
    eval("5*-3", -15.0)
    eval("2+-3", -1.0)
    eval("2--3", 5.0)
    eval("3/2+2/-4-3*0.5", -0.5)
    intercept[ComparisonFailException](eval("-", Double.NaN))
  }

  test("Unary and binary minus") {
    eval("3--3", 6.0)
    eval("3+-3", 0.0)
    eval("3- -3", 6.0)
    eval("3 - - 3", 6.0)
  }

  test("Assignments") {
    eval("a = 3", 3.0)
    eval("_a = 3", 3.0)
    eval("a_ = 3", 3.0)
    eval("a_b = 3", 3.0)
    eval("a1 = 3", 3.0)
    intercept[ComparisonFailException](eval("1a = 3", Double.NaN))
    eval("a = 3 + 4", 7.0)
    eval("a = b = 3", 3.0)
  }