package replcalc.eval

final class Dictionary(private var expressions: Map[String, Expression] = Map.empty):
  def add(name: String, expr: Expression): Boolean =
    if expressions.contains(name) then false
    else 
      expressions += name -> expr
      true

  inline def get(name: String): Option[Expression] = expressions.get(name)

  inline def contains(name: String): Boolean = expressions.contains(name)

  inline def listNames: Set[String] = expressions.keySet
