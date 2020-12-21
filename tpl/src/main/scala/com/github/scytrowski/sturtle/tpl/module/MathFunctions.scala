package com.github.scytrowski.sturtle.tpl.module

import com.github.scytrowski.sturtle.tpl.interpreter.{FunctionSignature, VariableSignature}
import com.github.scytrowski.sturtle.tpl.types.Nat._

object MathFunctions {
  def re = FunctionSignature("re", _1)
  def im = FunctionSignature("im", _1)
  def con = FunctionSignature("con", _1)
  def abs = FunctionSignature("abs", _1)
  def arg = FunctionSignature("arg", _1)
  def floor = FunctionSignature("floor", _1)
  def ceil = FunctionSignature("ceil", _1)
  def min = FunctionSignature("min", _2)
  def max = FunctionSignature("max", _2)
  def pow = FunctionSignature("pow", _2)
  def exp = FunctionSignature("exp", _1)
  def log = FunctionSignature("log", _2)
  def ln = FunctionSignature("ln", _1)
  def sin = FunctionSignature("sin", _1)
  def cos = FunctionSignature("cos", _1)
}
