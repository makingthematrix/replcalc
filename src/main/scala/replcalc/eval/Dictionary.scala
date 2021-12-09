package replcalc.eval

class Dictionary(private var expressions: Map[String, Expression] = Map.empty):
  def add(name: String, expression: Expression): Boolean =
    if expressions.contains(name) then false
    else 
      expressions += name -> expression
      true

  def get(name: String): Option[Expression] = expressions.get(name)
  
  def contains(name: String): Boolean = expressions.contains(name)
  
  def listNames: Set[String] = expressions.keySet
