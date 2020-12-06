package com.github.scytrowski.sturtle.tpl.interpreter

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction._
import com.github.scytrowski.sturtle.tpl.interpreter.Value.BooleanValue

final class TPLInterpreter[F[_]: MonadError[*[_], Throwable]] extends Interpreter[F, TPLCode] {
  override def interpret(code: TPLCode, ctx: InterpreterContext): F[InterpreterContext] = interpretCode(code, ctx)

  private def interpretValue(code: TPLCode.WithPush, ctx: InterpreterContext): F[(Value, InterpreterContext)] =
    interpretCode(code, ctx).flatMap(c => wrapResult(c.pop))

  // fixme: Rewrite with no recursion
  private def interpretCode(code: TPLCode, ctx: InterpreterContext): F[InterpreterContext] =
    code.headOption match {
      case Some(PushValue(value)) => ctx.pushValue(value).pure.flatMap(interpretCode(code.tail, _))
      case Some(PushFrom(from)) => wrapResult(ctx.pushFrom(from)).flatMap(interpretCode(code.tail, _))
      case Some(Invoke(signature)) =>
        for {
          function   <- wrapResult(ctx.getFunction(signature))
          updatedCtx <- interpretCode(function.code, ctx)
        } yield updatedCtx
      case Some(PopTo(to)) => wrapResult(ctx.popTo(to)).flatMap(interpretCode(code.tail, _))
      case Some(branch: Branch) => interpretBranch(branch, ctx)
      case Some(loop: Loop) => interpretLoop(loop, ctx)
      case Some(DefineFunction(signature, body)) => ctx.putFunction(signature, body).pure
      case Some(ExitLoop) => wrapResult(ctx.exitLoop).flatMap(interpretCode(code.tail, _))
      case Some(ExitFunction(push)) =>
        interpretCode(TPLCode(push), ctx)
          .flatMap(c => wrapResult(c.exitFunction))
          .flatMap(interpretCode(code.tail, _))
      case None => ctx.pure
    }

  private def interpretBranch(branch: Branch, ctx: InterpreterContext): F[InterpreterContext] = {
    // fixme: Rewrite with no recursion
    def interpretBranchRec(ctx: InterpreterContext, casesLeft: List[BranchCase]): F[InterpreterContext] =
      casesLeft.headOption match {
        case Some(c) =>
          interpretValue(c.condition, ctx).flatMap {
            case (BooleanValue(true), updatedCtx) => interpretCode(c.body, updatedCtx)
            case (_, updatedCtx) => interpretBranchRec(updatedCtx, casesLeft.tail)
          }
        case None    => ctx.pure
      }

    interpretBranchRec(ctx, branch.cases.toList)
  }

  private def interpretLoop(loop: Loop, ctx: InterpreterContext): F[InterpreterContext] = {
    // fixme: Rewrite with no recursion
    def interpretLoopRec(ctx: InterpreterContext): F[InterpreterContext] =
      interpretValue(loop.condition, ctx).flatMap {
        case (BooleanValue(true), updatedCtx) => interpretCode(loop.body, updatedCtx).flatMap(interpretLoopRec)
        case (_, updatedCtx) => updatedCtx.pure
      }

    interpretLoopRec(ctx)
  }

  private def wrapResult[A](result: Either[InterpreterError, A]): F[A] =
    result match {
      case Left(error) => InterpreterException(error).raiseError
      case Right(a) => a.pure
    }
}
