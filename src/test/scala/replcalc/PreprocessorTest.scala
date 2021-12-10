package replcalc

import munit.Location

class PreprocessorTest extends munit.FunSuite:
  implicit val location: Location = Location.empty

  private def pre = Parser().preprocessor

  private def evalParens(line: String, prefix: String = "", suffix: String = ""): Unit =
    pre.process(line) match
      case Left(error) =>
        fail(s"Parsing error: ${error.msg}")
      case Right(result) =>
        assert(result.startsWith(prefix))
        assert(result.endsWith(suffix))
        assert(!result.contains("("))
        assert(!result.contains(")"))
        assert(result.length > prefix.length + suffix.length)

  private def shouldFailParens(line: String): Unit =
    pre.process(line) match
      case Left(error) =>
      case Right(result) => fail(s"Unfortunately this is working just fine: $line")

  test("Do nothing if the line does not contain whitespaces") {
    val line = "abcdef"
    assertEquals(pre.process(line), Right(line))
  }

  test("Remove whitespaces from the line") {
    assertEquals(pre.process("abc def"), Right("abcdef"))
    assertEquals(pre.process(" abc def"), Right("abcdef"))
    assertEquals(pre.process("abc def "), Right("abcdef"))
    assertEquals(pre.process(" ab cd ef "), Right("abcdef"))
  }

  test("Replace parentheses with a special value") {
    evalParens("1+(2+3)+4", "1+", "+4")
    evalParens("(1+2)")
    evalParens("(1+2)+3", "", "+3")
    evalParens("1+(2+3)", "1+", "")
  }

  test("Handle more than one set of parentheses") {
    evalParens("1+(2+3)+(4+5)+6", "1+", "+6")
    evalParens("(1+2)+3+(4+5)")
    evalParens("(1+2)+(4+5)")
    evalParens("1+(2+3)+(4+5)", "1+")
    evalParens("(2+3)+(4+5)+6", "", "+6")
    evalParens("1+(2+3)+(4+5)+(6+7)+8", "1+", "+8")
  }

  test("Handle nested parentheses") {
    evalParens("1+(2+(3+4)+5)+6", "1+", "+6")
    evalParens("(1+(2+(3+4)+5))+6", "", "+6")
    evalParens("1+((2+(3+4)+5)+6)", "1+", "")
  }

  test("Handle unclosed paretheses") {
    shouldFailParens("(")
    shouldFailParens("((")
    shouldFailParens("(1+2)+(")
    shouldFailParens("(1+(2+(3+4)+5)+6")
    shouldFailParens("1+((2+(3+4)+5)+6")
  }
