package replcalc.eval

import scala.annotation.tailrec

sealed trait Expression:
  def evaluate: Double

final case class Constant(number: Double) extends Expression:
  override def evaluate: Double = number

final case class Text(text: String) extends Expression:
  override def evaluate: Double = parse.evaluate

  private def parse: Expression =
    Constant(text.toDoubleOption.getOrElse(Double.NaN))

object Expression:
  def apply(text: String): Expression = Text(text)
