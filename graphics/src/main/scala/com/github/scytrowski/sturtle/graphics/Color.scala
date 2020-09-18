package com.github.scytrowski.sturtle.graphics

final case class Color private(red: Short, green: Short, blue: Short, bitDepth: Byte)

object Color {
  val black: Color = Color.rgb(0, 0 ,0)

  def rgb(red: Short, green: Short, blue: Short, bitDepth: Byte = 8): Color = {
    val maxValue = (1 << bitDepth) - 1
    val normalize = normalizeComponent(maxValue, _)
    Color(normalize(red), normalize(green), normalize(blue), bitDepth)
  }

  private def normalizeComponent(maxValue: Int, component: Short): Short =
    if (component < 0)
      0
    else if (component > maxValue)
      maxValue.toShort
    else
      component
}