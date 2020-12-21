package com.github.scytrowski.sturtle.tpl.interpreter

import cats.data.OptionT
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Monad, MonadError}
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction._
import com.github.scytrowski.sturtle.tpl.syntax.foldable._
import com.github.scytrowski.sturtle.tpl.syntax.monad._
import com.github.scytrowski.sturtle.tpl.types.Nat

final class TPLInterpreter[F[_]: MonadError[*[_], Throwable]] extends Interpreter[F, TPLCode] {
  override def interpret(code: TPLCode, ctx: InterpreterContext[F]): F[InterpreterContext[F]] = interpretCode(code, ctx)

  private def interpretValue(code: TPLCode.WithPush, ctx: InterpreterContext[F]): F[(Value, InterpreterContext[F])] =
    interpretCode(code, ctx).flatMap(c => wrapResult(c.pop))

  private def interpretCode(code: TPLCode, ctx: InterpreterContext[F]): F[InterpreterContext[F]] = {
    code.instructions.foldLeftWhileM(ctx) { case (ctx, ins) =>
      if (ctx.isInValidScope)
        interpretInstruction(ins, ctx).map(Some(_))
      else
        Option.empty[InterpreterContext[F]].pure
    }
  }

  private def interpretInstruction(ins: TPLInstruction, ctx: InterpreterContext[F]): F[InterpreterContext[F]] =
    ins match {
      case push: Push => interpretPush(push, ctx)
      case PopTo(to) => wrapResult(ctx.popTo(to))
      case branch: Branch => interpretBranch(branch, ctx)
      case loop: Loop => interpretLoop(loop, ctx)
      case DefineFunction(signature, body) => ctx.putObject(RuntimeFunction.Stored[F, signature.ParamsN](signature, body)).pure
      case ExitLoop => wrapResult(ctx.exitLoop)
      case ExitFunction(push) => interpretPush(push, ctx).flatMap(c => wrapResult(c.exitFunction))
    }

  private def interpretPush(push: Push, ctx: InterpreterContext[F]): F[InterpreterContext[F]] =
    push match {
      case PushValue(value) => ctx.pushValue(value).pure
      case PushFrom(from) => wrapResult(ctx.pushFrom(from))
      case Invoke(signature) => invokeFunction(signature, ctx)
    }

  private def interpretBranch(branch: Branch, ctx: InterpreterContext[F]): F[InterpreterContext[F]] = {
    OptionT {
      Monad[F]
        .findFirst(branch.cases)(c => interpretValue(c.condition, ctx).map {
          case (BooleanValue(true), _) => true
          case _ => false
        })
    }.semiflatMap(c => interpretCode(c.body, ctx)).getOrElse(ctx)
  }

  private def interpretLoop(loop: Loop, ctx: InterpreterContext[F]): F[InterpreterContext[F]] =
    ctx.enterLoop.pure.foldWhileM { ctx =>
      OptionT(interpretValue(loop.condition, ctx).collect { case (BooleanValue(true), ctx2) => ctx2 })
        .semiflatMap(interpretCode(loop.body, _))
        .value
    }.flatMap(ctx2 => wrapResult(ctx2.exitLoop)).map(ctx3 => ctx3.copy(scopeId = ctx.scopeId, scope = ctx3.scope.withType(ctx.scope.scopeType)))

  private def invokeFunction(signature: FunctionSignature, ctx: InterpreterContext[F]): F[InterpreterContext[F]] =
    for {
      func <- wrapResult(ctx.getFunction(signature))
      ctx2 <- func.invoke(this, ctx.enterFunction)
      ctx3 <- wrapResult(ctx2.exitFunction)
    } yield ctx3.copy(scopeId = ctx.scopeId)

  private def wrapResult[A](result: Either[InterpreterError, A]): F[A] =
    result match {
      case Left(error) => InterpreterException(error).raiseError
      case Right(a) => a.pure
    }
}
