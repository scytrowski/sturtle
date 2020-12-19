package com.github.scytrowski.sturtle.tpl.interpreter

sealed abstract class ScopeType {
  def id: String
}

object ScopeType {
  final case class Regular(id: String) extends ScopeType
  final case class WithinLoop(id: String) extends ScopeType
  final case class WithinFunction(id: String) extends ScopeType
}
