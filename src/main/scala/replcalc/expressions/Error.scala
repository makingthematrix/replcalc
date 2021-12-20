package replcalc.expressions

enum Error(val msg: String, private val title: String):
  case ParsingError(override val msg: String) extends Error(msg, "Parsing error")
  case EvaluationError(override val msg: String) extends Error(msg, "Evaluation error")
  case PreprocessorError(override val msg: String) extends Error(msg, "Preprocessor error")

  override def toString: String = s"$title: $msg"
end Error