package replcalc

import munit.Location

class PreprocessorTest extends munit.FunSuite:
  implicit val location: Location = Location.empty

  private def evalParens(line: String, prefix: String = "", suffix: String = "")(implicit parser: Parser = Parser()): Unit =
    parser.preprocessor.process(line) match
      case Left(error) =>
        fail(s"Parsing error: ${error.msg}")
      case Right(result) =>
        assert(result.startsWith(prefix))
        assert(result.endsWith(suffix))
        if !result.contains('=') then
          assert(!result.contains('('))
          assert(!result.contains(')'))
        else
          val rightPart = result.substring(result.indexOf('='))
          assert(!rightPart.contains('('))
          assert(!rightPart.contains(')'))
        assert(result.length > prefix.length + suffix.length)

  private def shouldFailParens(line: String)(implicit parser: Parser = Parser()): Unit =
    parser.preprocessor.process(line) match
      case Left(error) =>
      case Right(result) => fail(s"Unfortunately this is working just fine: $line")

  test("Do nothing if the line does not contain whitespaces") {
    val pre = Parser().preprocessor
    val line = "abcdef"
    assertEquals(pre.process(line), Right(line))
  }

  test("Remove whitespaces from the line") {
    val pre = Parser().preprocessor
    assertEquals(pre.process("abc def"), Right("abcdef"))
    assertEquals(pre.process(" abc def"), Right("abcdef"))
    assertEquals(pre.process("abc def "), Right("abcdef"))
    assertEquals(pre.process(" ab cd ef "), Right("abcdef"))
  }

  test("Replace parentheses with a special value") {
    implicit val parser: Parser = Parser()
    evalParens("1+(2+3)+4", "1+", "+4")
    evalParens("(1+2)")
    evalParens("(1+2)+3", "", "+3")
    evalParens("1+(2+3)", "1+", "")
  }

  test("Parentheses with assignments") {
    implicit val parser: Parser = Parser()
    evalParens("a = 1 + (2 + 3) + 4", "a=1+", "+4")
    evalParens("b = (1 + 2)")
    evalParens("c = (1 + 2) + 3", "c=", "+3")
    evalParens("d = 1+ (2 + 3)", "d=1+", "")
    evalParens("foo(x) = 1 + (2 + 3)", "foo(x)=1+", "")
  }

  test("Handle more than one set of parentheses") {
    implicit val parser: Parser = Parser()
    evalParens("1+(2+3)+(4+5)+6", "1+", "+6")
    evalParens("(1+2)+3+(4+5)")
    evalParens("(1+2)+(4+5)")
    evalParens("1+(2+3)+(4+5)", "1+")
    evalParens("(2+3)+(4+5)+6", "", "+6")
    evalParens("1+(2+3)+(4+5)+(6+7)+8", "1+", "+8")
  }

  test("Handle nested parentheses") {
    implicit val parser: Parser = Parser()
    evalParens("1+(2+(3+4)+5)+6", "1+", "+6")
    evalParens("(1+(2+(3+4)+5))+6", "", "+6")
    evalParens("1+((2+(3+4)+5)+6)", "1+", "")
  }

  test("Handle unclosed paretheses") {
    implicit val parser: Parser = Parser()
    shouldFailParens("(")
    shouldFailParens("((")
    shouldFailParens("(1+2)+(")
    shouldFailParens("(1+(2+(3+4)+5)+6")
    shouldFailParens("1+((2+(3+4)+5)+6")
  }

  test("Ignore function parentheses") {
    val pre = Parser().preprocessor
    assertEquals(pre.process("foo(1)"), Right("foo(1)"))
    assertEquals(pre.process("foo(1)+1"), Right("foo(1)+1"))
    assertEquals(pre.process("2+foo(3,4)"), Right("2+foo(3,4)"))

    pre.process("2+foo((3+1),4)") match
      case Right(result) =>
        assert(result.startsWith("2+foo("))
        assert(result.endsWith(",4)"))
        assert(!result.contains("(3+1)"))
      case Left(error) =>
        fail(error.msg)

    pre.process("foo((1),(2))") match
      case Right(result) =>
        assert(result.contains("foo("))
        val index = result.indexOf("foo(") + 5
        assert(result.substring(index).contains(')'))
        assert(!result.contains("(1)"))
        assert(!result.contains("(2)"))
      case Left(error) =>
        fail(error.msg)

    pre.process("(1+2)+foo((3+1),(4+5))+(6+7)") match
      case Right(result) =>
        assert(result.contains("+foo("))
        val index = result.indexOf("+foo(") + 5
        assert(result.substring(index).contains(')'))
        assert(!result.contains("(3+1)"))
        assert(!result.contains("(4+5)"))
      case Left(error) =>
        fail(error.msg)
  }
