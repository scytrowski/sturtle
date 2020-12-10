package com.github.scytrowski.sturtle.tpl.module

import com.github.scytrowski.sturtle.tpl.interpreter.FunctionSignature
import shapeless.nat._

object SpecialFunctions {
  val equal = FunctionSignature("_eq", _2)
  val notEqual = FunctionSignature("_neq", _2)
  val less = FunctionSignature("_le", _2)
  val lessOrEqual = FunctionSignature("leq", _2)
  val greater = FunctionSignature("_gre", _2)
  val greaterOrEqual = FunctionSignature("_greq", _2)
  val negate = FunctionSignature("_neg", _1)
  val and = FunctionSignature("_and", _2)
  val or = FunctionSignature("_or", _2)
  val plus = FunctionSignature("_plus", _1)
  val add = FunctionSignature("_add", _2)
  val minus = FunctionSignature("_minus", _1)
  val sub = FunctionSignature("_sub", _2)
  val multi = FunctionSignature("_multi", _2)
  val div = FunctionSignature("_div", _2)
  val point = FunctionSignature("_point", _2)
  val vector = FunctionSignature("_vector", _2)
  val angle = FunctionSignature("_angle", _1)
  val color = FunctionSignature("_color", _3)
  val bool = FunctionSignature("_bool", _1)
}
