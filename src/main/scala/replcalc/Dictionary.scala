package replcalc

import replcalc.Dictionary.isValidName
import replcalc.expressions.{Expression, Assignment}

final class Dictionary(private var dict: Map[String, Expression] = Map.empty,
                       private var specialValuesCounter: Long = 0L):
  def canAssign(name: String): Boolean =
    dict.get(name) match
      case Some(_ : Assignment)      => true
      case None if isValidName(name) => true
      case _                         => false
  
  def add(name: String, expr: Expression): Boolean =
    dict.get(name) match
      case Some(_ : Assignment) =>
        dict += name -> expr
        true
      case None if isValidName(name) =>
        dict += name -> expr
        true
      case _ =>
        false

  def addSpecial(expr: Expression): String =
    specialValuesCounter += 1
    val name = s"$$$specialValuesCounter"
    dict += name -> expr
    name
  
  inline def get(name: String): Option[Expression] = dict.get(name)

  inline def contains(name: String): Boolean = dict.contains(name)

  inline def expressions: Map[String, Expression] = dict.filter(_._1.head != '$')

  inline def specials: Map[String, Expression] = dict.filter(_._1.head == '$')
  
  def clean(): Unit =
    dict = dict.view.filterKeys(_.head != '$').toMap
  
  def copy(updates: Map[String, Expression]): Dictionary =
    Dictionary(dict ++ updates, specialValuesCounter)
  
object Dictionary:
  def isValidName(name: String, canBeSpecial: Boolean = false): Boolean =
    name.nonEmpty &&
      name.exists(_ != '_') &&
      (name.head.isLetter || name.head == '_' || (canBeSpecial && name.head == '$')) &&
      name.substring(1).forall(ch => ch.isLetterOrDigit || ch == '_')