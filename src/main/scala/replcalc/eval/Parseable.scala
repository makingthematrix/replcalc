package replcalc.eval

// feels good, might delete later
trait Parseable[T <: Expression]:
  def parse(text: String): Option[T]
