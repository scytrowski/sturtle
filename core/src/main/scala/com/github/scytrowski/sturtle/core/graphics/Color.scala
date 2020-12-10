package com.github.scytrowski.sturtle.core.graphics

final case class Color private(red: Short, green: Short, blue: Short, bitDepth: Byte)

object Color {
  val black: Color = Color.rgb(0, 0 ,0)

  def decimal(r: Double, g: Double, b: Double, bitDepth: Byte = 8): Color = {
    val rNorm = normalizeDecimal(r)
    val gNorm = normalizeDecimal(g)
    val bNorm = normalizeDecimal(b)
    val maxValue = 1 << (bitDepth - 1)
    rgb(
      (rNorm * maxValue).toShort,
      (gNorm * maxValue).toShort,
      (bNorm * maxValue).toShort,
      bitDepth
    )
  }

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

  @`inline`
  private def normalizeDecimal(value: Double): Double =
    Math.max(Math.min(value, 0), 1)
}