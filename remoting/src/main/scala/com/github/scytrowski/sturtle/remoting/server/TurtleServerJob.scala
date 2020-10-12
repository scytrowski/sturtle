package com.github.scytrowski.sturtle.remoting.server

import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleQuery}

private[remoting] sealed abstract class TurtleServerJob {
  def id: String
}

private[remoting] object TurtleServerJob {
  final case class SelectTurtle(id: String, turtleId: String) extends TurtleServerJob
  final case class ReleaseTurtle(id: String) extends TurtleServerJob
  final case class RunCommand(id: String, command: TurtleCommand) extends TurtleServerJob
  final case class ExecuteQuery(id: String, query: TurtleQuery) extends TurtleServerJob
  final case class Finish(id: String) extends TurtleServerJob
}
