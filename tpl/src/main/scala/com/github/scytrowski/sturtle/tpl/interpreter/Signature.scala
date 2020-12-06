package com.github.scytrowski.sturtle.tpl.interpreter

sealed abstract class Signature {
  def name: String
}

final case class VariableSignature(name: String) extends Signature
final case class FunctionSignature(name: String, parametersCount: Int) extends Signature
