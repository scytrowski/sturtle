package com.github.scytrowski.sturtle.tpl.interpreter

import cats.Foldable
import cats.syntax.foldable._
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError._
import com.github.scytrowski.sturtle.tpl.util.IdGenerator

final case class InterpreterContext[F[_]](scopeId: String, scope: Scope[F], stack: Stack[Value]) {
  def isInValidScope: Boolean = scope.scopeType.id == scopeId

  def getVariable(signature: VariableSignature): Either[InterpreterError, RuntimeVariable] =
    scope
      .getVariable(signature)
      .toRight(VariableNotFound(signature))

  def getFunction(signature: FunctionSignature): Either[InterpreterError, RuntimeFunction[F]] =
    scope
      .getFunction(signature)
      .toRight(FunctionNotFound(signature))

  def putObjects[G[_]: Foldable](objects: G[RuntimeObject[F]]): InterpreterContext[F] =
    objects.foldLeft(this)(_.putObject(_))

  def putObject(obj: RuntimeObject[F]): InterpreterContext[F] = copy(scope = scope.putObject(obj))

  def pushValue(value: Value): InterpreterContext[F] = copy(stack = stack.push(value))

  def pushFrom(from: VariableSignature): Either[InterpreterError, InterpreterContext[F]] =
    getVariable(from)
      .map(_.value)
      .map(pushValue)

  def pop: Either[InterpreterError, (Value, InterpreterContext[F])] =
    stack.pop
      .toRight(EmptyStack)
      .map { case (value, updatedStack) => value -> copy(stack = updatedStack) }

  def popTo(to: VariableSignature): Either[InterpreterError, InterpreterContext[F]] =
    pop
      .map { case (value, updatedCtx) => updatedCtx.putObject(RuntimeVariable(to, value)) }

  def enterFunction: InterpreterContext[F] = {
    val newScopeId = IdGenerator.generate
    copy(
      scopeId = newScopeId,
      scope = scope.onTop.withinFunction(newScopeId)
    )
  }

  def enterLoop: InterpreterContext[F] = {
    val newScopeId = IdGenerator.generate
    copy(
      scopeId = newScopeId,
      scope = scope.withinLoop(newScopeId)
    )
  }

  def exitFunction: Either[InterpreterError, InterpreterContext[F]] =
    if (isInValidScope)
      scope.scopeType match {
        case _: ScopeType.WithinFunction => Right(copy(scope = scope.parent))
        case invalid => Left(NotInFunction(invalid))
      }
    else
      Right(this)

  def exitLoop: Either[InterpreterError, InterpreterContext[F]] =
    if (isInValidScope)
      scope.scopeType match {
        case _: ScopeType.WithinLoop => Right(this)
        case invalid => Left(NotInLoop(invalid))
      }
    else Right(this)

  def <+>(other: InterpreterContext[F]): InterpreterContext[F] = merge(other)

  def merge(other: InterpreterContext[F]): InterpreterContext[F] =
    InterpreterContext(other.scopeId, scope.merge(other.scope), stack.merge(other.stack))
}

object InterpreterContext {
  def initial[F[_]]: InterpreterContext[F] = {
    val scopeId = IdGenerator.generate
    InterpreterContext(scopeId, Scope.root(scopeId), Stack.empty)
  }
}
