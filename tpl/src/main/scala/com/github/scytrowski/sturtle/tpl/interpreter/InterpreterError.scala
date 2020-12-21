package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.core.{TurtleQuery, TurtleQueryAnswer}
import com.github.scytrowski.sturtle.tpl.types.Nat

sealed abstract class InterpreterError

object InterpreterError {
  final case class VariableNotFound(signature: VariableSignature) extends InterpreterError
  final case class FunctionNotFound(signature: FunctionSignature) extends InterpreterError
  final case class IllegalParameter(functionSignature: FunctionSignature, index: Nat, value: Value) extends InterpreterError
  final case class InvalidAnswer(query: TurtleQuery, answer: TurtleQueryAnswer) extends InterpreterError
  case object EmptyStack extends InterpreterError
  final case class NotInFunction(actualScopeType: ScopeType) extends InterpreterError
  final case class NotInLoop(actualScopeType: ScopeType) extends InterpreterError
  case object DivisionByZero extends InterpreterError
  final case class InternalError(cause: Throwable) extends InterpreterError
}
