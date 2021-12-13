package replcalc

import replcalc.Dictionary.isValidName
import replcalc.expressions.Expression

final class Dictionary(private var expressions: Map[String, Expression] = Map.empty,
                       private var specialValuesCounter: Long = 0L):
  def add(name: String, expr: Expression): Boolean =
    if !isValidName(name) || expressions.contains(name) then false
    else
      expressions += name -> expr
      true

  def addSpecial(expr: Expression): String =
    specialValuesCounter += 1
    val name = s"$$$specialValuesCounter"
    expressions += name -> expr
    name
  
  inline def get(name: String): Option[Expression] = expressions.get(name)

  inline def contains(name: String): Boolean = expressions.contains(name)

  inline def listNames(withSpecial: Boolean = false): Set[String] =
    if withSpecial then
      expressions.keySet
    else
      expressions.keySet.filter(_.head != '$')

  def copy(updates: Map[String, Expression]): Dictionary = 
    Dictionary(expressions ++ updates, specialValuesCounter)
  
object Dictionary:
  def isValidName(name: String, canBeSpecial: Boolean = false): Boolean =
    name.nonEmpty &&
      (name.head.isLetter || name.head == '_' || (canBeSpecial && name.head == '$')) &&
      name.substring(1).forall(ch => ch.isLetterOrDigit || ch == '_')