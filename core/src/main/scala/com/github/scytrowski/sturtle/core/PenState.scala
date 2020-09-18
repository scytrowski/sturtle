package com.github.scytrowski.sturtle.core

sealed abstract class PenState

object PenState {
  case object Down extends PenState
  case object Up extends PenState
}