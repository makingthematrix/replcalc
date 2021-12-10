package replcalc.eval

import munit.Location

class PreprocessorTest extends munit.FunSuite:
  implicit val location: Location = Location.empty

  test("Do nothing if the line does not contain whitespaces") {
    val pre = Preprocessor()
    val line = "abcdef"
    assertEquals(pre.process(line), line)
  }

  test("Remove whitespaces from the line") {
    val pre = Preprocessor()
    assertEquals(pre.process("abc def"), "abcdef")
    assertEquals(pre.process(" abc def"), "abcdef")
    assertEquals(pre.process("abc def "), "abcdef")
    assertEquals(pre.process(" ab cd ef "), "abcdef")
  }