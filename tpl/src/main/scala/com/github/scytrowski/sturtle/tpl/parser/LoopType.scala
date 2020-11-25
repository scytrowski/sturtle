package com.github.scytrowski.sturtle.tpl.parser

sealed abstract class LoopType

object LoopType {
  case object While extends LoopType
  case object Repeat extends LoopType
}
