package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.{EmptyStack, FunctionNotFound, NotInFunction, NotInLoop, VariableNotFound}

final case class InterpreterContext(scope: Scope, stack: Stack[Value]) {
  def getVariable(signature: VariableSignature): Either[InterpreterError, RuntimeVariable] =
    scope
      .getVariable(signature)
      .toRight(VariableNotFound(signature))

  def putVariable(signature: VariableSignature, value: Value): InterpreterContext =
    copy(scope = scope.putObject(RuntimeVariable(signature, value)))

  def getFunction(signature: FunctionSignature): Either[InterpreterError, RuntimeFunction] =
    scope
      .getFunction(signature)
      .toRight(FunctionNotFound(signature))

  def putFunction(signature: FunctionSignature, body: TPLCode.WithExit): InterpreterContext =
    copy(scope = scope.putObject(RuntimeFunction(signature, body)))

  def pushValue(value: Value): InterpreterContext = copy(stack = stack.push(value))

  def pushFrom(from: VariableSignature): Either[InterpreterError, InterpreterContext] =
    getVariable(from)
      .map(_.value)
      .map(pushValue)

  def pop: Either[InterpreterError, (Value, InterpreterContext)] =
    stack.pop
      .toRight(EmptyStack)
      .map { case (value, updatedStack) => value -> copy(stack = updatedStack) }

  def popTo(to: VariableSignature): Either[InterpreterError, InterpreterContext] =
    pop
      .map { case (value, updatedCtx) => updatedCtx.putVariable(to, value) }

  def enterFunction: InterpreterContext = copy(scope = scope.onTop.withinFunction)

  def enterLoop: InterpreterContext = copy(scope = scope.onTop.withinLoop)

  def exitFunction: Either[InterpreterError, InterpreterContext] =
    scope.scopeType match {
      case ScopeType.WithinFunction => Right(copy(scope = scope.parent))
      case _ => Left(NotInFunction)
    }

  def exitLoop: Either[InterpreterError, InterpreterContext] =
    scope.scopeType match {
      case ScopeType.WithinLoop => Right(copy(scope = scope.parent))
      case _ => Left(NotInLoop)
    }
}

object InterpreterContext {
  val initial: InterpreterContext = InterpreterContext(Scope.root, Stack.empty)
}
