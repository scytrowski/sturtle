package com.github.scytrowski.sturtle.tpl.interpreter

import cats.{Foldable, UnorderedFoldable}
import cats.syntax.foldable._
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError._

final case class InterpreterContext[F[_]](scope: Scope[F], stack: Stack[Value]) {
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

  def enterFunction: InterpreterContext[F] = copy(scope = scope.onTop.withinFunction)

  def enterLoop: InterpreterContext[F] = copy(scope = scope.onTop.withinLoop)

  def exitFunction: Either[InterpreterError, InterpreterContext[F]] =
    scope.scopeType match {
      case ScopeType.WithinFunction => Right(copy(scope = scope.parent))
      case _ => Left(NotInFunction)
    }

  def exitLoop: Either[InterpreterError, InterpreterContext[F]] =
    scope.scopeType match {
      case ScopeType.WithinLoop => Right(copy(scope = scope.parent))
      case _ => Left(NotInLoop)
    }

  def <+>(other: InterpreterContext[F]): InterpreterContext[F] = merge(other)

  def merge(other: InterpreterContext[F]): InterpreterContext[F] =
    InterpreterContext(scope.merge(other.scope), stack.merge(other.stack))
}

object InterpreterContext {
  def initial[F[_]]: InterpreterContext[F] = InterpreterContext(Scope.root, Stack.empty)
}
