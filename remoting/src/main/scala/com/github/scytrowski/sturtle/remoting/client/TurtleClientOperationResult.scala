package com.github.scytrowski.sturtle.remoting.client

import com.github.scytrowski.sturtle.core.TurtleQueryAnswer

private[remoting] sealed abstract class TurtleClientOperationResult

private[remoting] object TurtleClientOperationResult {
  case object TurtleSelected extends TurtleClientOperationResult
  case object TurtleReleased extends TurtleClientOperationResult
  case object CommandRan extends TurtleClientOperationResult
  final case class QueryExecuted(answer: TurtleQueryAnswer) extends TurtleClientOperationResult
  case object NoTurtleSelected extends TurtleClientOperationResult
}
