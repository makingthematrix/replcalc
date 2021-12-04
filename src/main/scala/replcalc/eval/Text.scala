package replcalc.eval

final case class Text(text: String) extends Expression:
  override def evaluate: Double = Expression(text).evaluate

object Text extends Parseable[Text]:
  override def parse(text: String): Option[Text] = Some(Text(text.trim))
