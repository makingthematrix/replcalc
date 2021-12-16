package replcalc

import replcalc.Dictionary.isValidName
import replcalc.expressions.{Expression, Assignment}

final class Dictionary(private var expressions: Map[String, Expression] = Map.empty,
                       private var specialValuesCounter: Long = 0L):
  def canAssign(name: String): Boolean =
    expressions.get(name) match
      case Some(_ : Assignment)      => true
      case None if isValidName(name) => true
      case _                         => false
  
  def add(name: String, expr: Expression): Boolean =
    expressions.get(name) match
      case Some(_ : Assignment) =>
        expressions += name -> expr
        true
      case None if isValidName(name) =>
        expressions += name -> expr
        true
      case _ =>
        false

  def addSpecial(expr: Expression): String =
    specialValuesCounter += 1
    val name = s"$$$specialValuesCounter"
    expressions += name -> expr
    name
  
  inline def get(name: String): Option[Expression] = expressions.get(name)

  inline def contains(name: String): Boolean = expressions.contains(name)

  inline def listNames: Set[String] = expressions.keySet.filter(_.head != '$')

  def copy(updates: Map[String, Expression]): Dictionary = 
    Dictionary(expressions ++ updates, specialValuesCounter)
  
object Dictionary:
  def isValidName(name: String, canBeSpecial: Boolean = false): Boolean =
    name.nonEmpty &&
      (name.head.isLetter || name.head == '_' || (canBeSpecial && name.head == '$')) &&
      name.substring(1).forall(ch => ch.isLetterOrDigit || ch == '_')