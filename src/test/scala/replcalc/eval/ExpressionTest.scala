package replcalc.eval

import munit.Location

class ExpressionTest extends munit.FunSuite:
  implicit val location: Location = Location.empty

  test("Number") {
    assertEqualsDouble(Expression("4").evaluate, 4.0, 0.001)
    assertEqualsDouble(Expression("4.12").evaluate, 4.12, 0.001)
    assertEqualsDouble(Expression("0").evaluate, 0.0, 0.00001)
    assertEqualsDouble(Expression("blah").evaluate, Double.NaN, 0.1)
  }

  test("Add") {
    assertEqualsDouble(Expression("1+1").evaluate, 2.0, 0.001)
    assertEqualsDouble(Expression("1+2+3").evaluate, 6.0, 0.001)
  }

  test("Substract") {
    assertEqualsDouble(Expression("2-1").evaluate, 1.0, 0.001)
    assertEqualsDouble(Expression("3-2-1").evaluate, 0.0, 0.001)
    assertEqualsDouble(Expression("1-2").evaluate, -1.0, 0.001)
  }

  test("Add and Substract") {
    assertEqualsDouble(Expression("3+2-1").evaluate, 4.0, 0.001)
    assertEqualsDouble(Expression("3-2+1").evaluate, 2.0, 0.001)
    assertEqualsDouble(Expression("3+2-1+4").evaluate, 8.0, 0.001)
    assertEqualsDouble(Expression("3-2+1-4").evaluate, -2.0, 0.001)
  }
