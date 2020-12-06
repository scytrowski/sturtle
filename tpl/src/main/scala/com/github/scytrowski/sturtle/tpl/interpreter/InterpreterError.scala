package com.github.scytrowski.sturtle.tpl.interpreter

sealed abstract class InterpreterError

object InterpreterError {
  final case class VariableNotFound(signature: VariableSignature) extends InterpreterError
  final case class FunctionNotFound(signature: FunctionSignature) extends InterpreterError
  case object EmptyStack extends InterpreterError
  case object NotInFunction extends InterpreterError
  case object NotInLoop extends InterpreterError
}
