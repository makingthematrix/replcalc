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

  test("Multiply") {
    assertEqualsDouble(Expression("1*1").evaluate, 1.0, 0.001)
    assertEqualsDouble(Expression("1*2*3").evaluate, 6.0, 0.001)
    assertEqualsDouble(Expression("5.0*2.5").evaluate, 12.5, 0.001)
    assertEqualsDouble(Expression("3.0*0").evaluate, 0.0, 0.001)
  }

  test("Divide") {
    assertEqualsDouble(Expression("2/1").evaluate, 2.0, 0.001)
    assertEqualsDouble(Expression("3/2/2").evaluate, 0.75, 0.001)
    assertEqualsDouble(Expression("1.0/2.0").evaluate, 0.5, 0.001)
  }

  test("Multiply and Divide") {
    assertEqualsDouble(Expression("3*2/1").evaluate, 6.0, 0.001)
    assertEqualsDouble(Expression("3/2*1").evaluate, 1.5, 0.001)
    assertEqualsDouble(Expression("3*2/2*4").evaluate, 12.0, 0.001)
    assertEqualsDouble(Expression("3/2*2/4").evaluate, 0.75, 0.001)
  }

  test("Add and Substract and Multiply and Divide") {
    assertEqualsDouble(Expression("1+3*2/1").evaluate, 7.0, 0.001)
    assertEqualsDouble(Expression("3/2*1+1").evaluate, 2.5, 0.001)
    assertEqualsDouble(Expression("0-3*2/2*4").evaluate, -12.0, 0.001)
    assertEqualsDouble(Expression("3/2+2/4-3*0.5").evaluate, 0.5, 0.001)
  }

  test("Unary minus") {
    assertEqualsDouble(Expression("-3").evaluate, -3.0, 0.001)
    assertEqualsDouble(Expression("5*-3").evaluate, -15.0, 0.001)
    assertEqualsDouble(Expression("2+-3").evaluate, -1.0, 0.001)
    assertEqualsDouble(Expression("2--3").evaluate, 5.0, 0.001)
    assertEqualsDouble(Expression("3/2+2/-4-3*0.5").evaluate, -0.5, 0.001)
  }