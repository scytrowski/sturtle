package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.core.{TurtleQuery, TurtleQueryAnswer}

sealed abstract class InterpreterError

object InterpreterError {
  final case class VariableNotFound(signature: VariableSignature) extends InterpreterError
  final case class FunctionNotFound(signature: FunctionSignature) extends InterpreterError
  final case class InvalidValue(value: Value) extends InterpreterError
  final case class InvalidAnswer(query: TurtleQuery, answer: TurtleQueryAnswer) extends InterpreterError
  case object EmptyStack extends InterpreterError
  case object NotInFunction extends InterpreterError
  case object NotInLoop extends InterpreterError
  case object DivisionByZero extends InterpreterError
  case object RealNumberExpected extends InterpreterError
}
