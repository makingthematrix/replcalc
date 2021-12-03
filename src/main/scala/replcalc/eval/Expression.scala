package replcalc.eval

import scala.annotation.tailrec

sealed trait Expression:
  def evaluate: Double

object Expression:
  def apply(text: String): Expression =
    val trimmed = text.trim
    Add.parse(trimmed)
      .orElse(Constant.parse(trimmed))
      .getOrElse(Constant(Double.NaN))

// feels good, might delete later
trait Parseable[T <: Expression]:
  def parse(text: String): Option[T]

final case class Constant(number: Double) extends Expression:
  override def evaluate: Double = number

object Constant extends Parseable[Constant]:
  def parse(text: String): Option[Constant] =
    text.trim.toDoubleOption.map(Constant.apply)

final case class Add(left: Expression, right: Expression) extends Expression:
  override def evaluate: Double = left.evaluate + right.evaluate

object Add extends Parseable[Add]:
  def parse(text: String): Option[Add] =
    text.trim.lastIndexOf("+") match
      case index if index > 0 && index < text.length - 1 =>
        Some(Add(Expression(text.substring(0, index)), Expression(text.substring(index + 1))))
      case _ => None

final case class Text(text: String) extends Expression:
  override def evaluate: Double = Expression(text).evaluate

object Text extends Parseable[Text]:
  def parse(text: String): Option[Text] = Some(Text(text.trim))
