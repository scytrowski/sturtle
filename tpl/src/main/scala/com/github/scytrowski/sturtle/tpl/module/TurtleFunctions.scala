package com.github.scytrowski.sturtle.tpl.module

import com.github.scytrowski.sturtle.tpl.interpreter.FunctionSignature
import com.github.scytrowski.sturtle.tpl.types.Nat._

object TurtleFunctions {
  val goto = FunctionSignature("goto", _1)
  val forward = FunctionSignature("forward", _1)
  val backward = FunctionSignature("backward", _1)
  val pos = FunctionSignature("pos", _0)
  val left = FunctionSignature("left", _1)
  val right = FunctionSignature("right", _1)
  val angle = FunctionSignature("angle", _0)
  val fill = FunctionSignature("fill", _0)
  val clear = FunctionSignature("clear", _0)
  val down = FunctionSignature("down", _0)
  val up = FunctionSignature("up", _0)
  val setPenColor = FunctionSignature("penColor", _1)
  val getPenColor = FunctionSignature("penColor", _0)
  val setFillColor = FunctionSignature("fillColor", _1)
  val getFillColor = FunctionSignature("fillColor", _0)
}
