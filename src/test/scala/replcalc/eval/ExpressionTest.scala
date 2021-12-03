package replcalc.eval

import munit.Location

class ExpressionTest extends munit.FunSuite:
  implicit val location: Location = Location.empty

  test("Number") {
    assertEqualsDouble(Expression("4").evaluate, 4.0, 0.001)
    assertEqualsDouble(Expression("-3").evaluate, -3.0, 0.001)
    assertEqualsDouble(Expression("4.12").evaluate, 4.12, 0.001)
    assertEqualsDouble(Expression("0").evaluate, 0.0, 0.00001)
    assertEqualsDouble(Expression("blah").evaluate, Double.NaN, 0.1)
  }

  test("Add") {
    assertEqualsDouble(Expression("1+1").evaluate, 2.0, 0.001)
    assertEqualsDouble(Expression("1+2+3").evaluate, 6.0, 0.001)
  }

