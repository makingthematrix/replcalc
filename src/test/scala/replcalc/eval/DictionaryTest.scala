package replcalc.eval

import munit.Location

class DictionaryTest extends munit.FunSuite:
  implicit val location: Location = Location.empty

  test("Add and get an expr") {
    val dict = Dictionary()
    dict.add("a", Constant(1.0))
    assertEquals(dict.get("a"), Some(Constant(1.0)))
  }

  test("Unable to reassign an expr") {
    val dict = Dictionary()
    assertEquals(dict.add("a", Constant(1.0)), true)
    assertEquals(dict.add("a", Constant(2.0)), false)
    assertEquals(dict.get("a"), Some(Constant(1.0)))
  }

  test("List values") {
    val dict = Dictionary()
    dict.add("a", Constant(1.0))
    dict.add("b", Constant(2.0))
    assertEquals(dict.listNames, Set("a", "b"))
  }