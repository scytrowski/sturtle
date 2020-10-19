package com.github.scytrowski.sturtle.es

sealed abstract class EventDescription[+S, +E] {
  def id: String

  def timestamp: Long
}

object EventDescription {
  final case class Event[+E](id: String, timestamp: Long, event: E) extends EventDescription[Nothing, E]
  final case class Snapshot[+S](id: String, timestamp: Long, state: S) extends EventDescription[S, Nothing]
}
