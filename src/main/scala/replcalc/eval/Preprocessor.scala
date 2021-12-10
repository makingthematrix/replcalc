package replcalc.eval

class Preprocessor:
  def process(line: String): String =
    removeWhitespaces(line)
    
  private def removeWhitespaces(line: String): String =
    if line.forall(!_.isWhitespace) then line
    else
      val sb = StringBuilder()
      line.foreach {
        case ch if !ch.isWhitespace => sb.addOne(ch)
        case _ =>
      }
      sb.toString
