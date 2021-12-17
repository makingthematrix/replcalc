package replcalc

import replcalc.Dictionary.isValidName
import replcalc.expressions.{Expression, Assignment}

final class Dictionary(private var map: Map[String, Expression] = Map.empty,
                       private var specialValuesCounter: Long = 0L):
  def canAssign(name: String): Boolean =
    map.get(name) match
      case Some(_ : Assignment)      => true
      case None if isValidName(name) => true
      case _                         => false
  
  def add(name: String, expr: Expression): Boolean =
    map.get(name) match
      case Some(_ : Assignment) =>
        map += name -> expr
        true
      case None if isValidName(name) =>
        map += name -> expr
        true
      case _ =>
        false

  def addSpecial(expr: Expression): String =
    specialValuesCounter += 1
    val name = s"$$$specialValuesCounter"
    map += name -> expr
    name
  
  inline def get(name: String): Option[Expression] = map.get(name)

  inline def contains(name: String): Boolean = map.contains(name)

  inline def expressions: Map[String, Expression] = map.filter(_._1.head != '$')

  inline def specials: Map[String, Expression] = map.filter(_._1.head == '$')
  
  def cleanSpecials(): Unit = map = map.view.filterKeys(_.head != '$').toMap
  
  def copy(updates: Map[String, Expression]): Dictionary = Dictionary(map ++ updates, specialValuesCounter)
  
object Dictionary:
  def isValidName(name: String, canBeSpecial: Boolean = false): Boolean =
    name.nonEmpty &&
      (name.head.isLetter || name.head == '_' || (canBeSpecial && name.head == '$')) &&
      name.substring(1).forall(ch => ch.isLetterOrDigit || ch == '_')