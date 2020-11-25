package com.github.scytrowski.sturtle.tpl.parser

import java.util.UUID

import com.github.scytrowski.sturtle.tpl.interpreter.SyntaxTree.Expression.Name

object SpecialNames {
  def temporaryVariable: Name = {
    val uuid = UUID.randomUUID()
    val tag = s"_${uuid.getMostSignificantBits}${uuid.getMostSignificantBits}"
    Name(tag)
  }

  val equal = Name("_eq")
  val notEqual = Name("_neq")
  val less = Name("_le")
  val lessOrEqual = Name("_leq")
  val greater = Name("_gre")
  val greaterOrEqual = Name("_greq")
  val negate = Name("_neg")
  val and = Name("_and")
  val or = Name("_or")
  val plus = Name("_plus")
  val add = Name("_add")
  val minus = Name("_minus")
  val sub = Name("_sub")
  val multi = Name("_multi")
  val div = Name("_div")
  val point = Name("_point")
  val vector = Name("_vector")
  val angle = Name("_angle")
  val color = Name("_color")
  val bool = Name("_bool")
}
