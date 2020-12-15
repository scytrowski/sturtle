package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.core.geometry.{Angle, Point, Vector}
import com.github.scytrowski.sturtle.core.graphics.Color
import com.github.scytrowski.sturtle.tpl.types.Complex

sealed abstract class Value

sealed abstract class LogicalValue(val logicalValue: Boolean) extends Value
sealed abstract class NumericValue(val numericValue: Complex) extends LogicalValue(numericValue == 1)

final case class BooleanValue(value: Boolean) extends LogicalValue(value)
final case class NumberValue(value: Complex) extends NumericValue(value)
final case class StringValue(value: String) extends Value
final case class PointValue(value: Point) extends Value
final case class VectorValue(value: Vector) extends Value
final case class AngleValue(value: Angle) extends NumericValue(Complex.real(value.value))
final case class ColorValue(value: Color) extends Value
case object VoidValue extends VoidValue

sealed abstract class VoidValue extends Value