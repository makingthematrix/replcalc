package replcalc.eval

final case class Text(text: String) extends Expression:
  override def evaluate: Option[Double] =
    Expression.parse(text).flatMap(_.evaluate)

object Text extends Parseable[Text]:
  override def parse(text: String): Option[Text] = Some(Text(text.trim))
