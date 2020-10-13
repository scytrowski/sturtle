package com.github.scytrowski.sturtle.es

sealed abstract class RecoveryData[+S, +E] {
  def id: String

  def timestamp: Long
}

object RecoveryData {
  final case class Event[+E](id: String, timestamp: Long, event: E) extends RecoveryData[Nothing, E]
  final case class Snapshot[+S](id: String, timestamp: Long, state: S) extends RecoveryData[S, Nothing]
}
