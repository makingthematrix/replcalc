package replcalc.eval

final case class Text(text: String) extends Expression:
  override def evaluate: Either[Error, Double] =
    Expression.parse(text.trim) match
      case Some(Right(expression)) => expression.evaluate
      case Some(Left(error))       => Left(error)
      case None                    => Left(EvaluationError(s"Text: Unable to evaluate $text"))

object Text extends Parseable[Text]:
  override def parse(text: String): ParsedExpr[Text] = Some(Right(Text(text.trim)))
