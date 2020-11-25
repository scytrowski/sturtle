package com.github.scytrowski.sturtle.tpl.interpreter

sealed abstract class Value

object Value {
  final case class BooleanValue(value: Boolean) extends Value
  final case class NumberValue(value: Double) extends Value
  final case class StringValue(value: String) extends Value
  case object VoidValue extends Value
}
