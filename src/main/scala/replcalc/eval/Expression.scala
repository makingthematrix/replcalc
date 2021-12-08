package replcalc.eval

trait Expression:
  def evaluate: Either[Error, Double]

