package com.github.scytrowski.sturtle.tpl.interpreter

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction._

final class TPLInterpreter[F[_]: MonadError[*[_], Throwable]] extends Interpreter[F, TPLCode] {
  override def interpret(code: TPLCode, ctx: InterpreterContext[F]): F[InterpreterContext[F]] = interpretCode(code, ctx)

  private def interpretValue(code: TPLCode.WithPush, ctx: InterpreterContext[F]): F[(Value, InterpreterContext[F])] =
    interpretCode(code, ctx).flatMap(c => wrapResult(c.pop))

  // fixme: Rewrite with no recursion
  private def interpretCode(code: TPLCode, ctx: InterpreterContext[F]): F[InterpreterContext[F]] =
    code.headOption match {
      case Some(PushValue(value)) => ctx.pushValue(value).pure.flatMap(interpretCode(code.tail, _))
      case Some(PushFrom(from)) => wrapResult(ctx.pushFrom(from)).flatMap(interpretCode(code.tail, _))
      case Some(Invoke(signature)) =>
        for {
          func <- wrapResult(ctx.getFunction(signature))
          res  <- func.invoke(this, ctx)
        } yield res
      case Some(PopTo(to)) => wrapResult(ctx.popTo(to)).flatMap(interpretCode(code.tail, _))
      case Some(branch: Branch) => interpretBranch(branch, ctx)
      case Some(loop: Loop) => interpretLoop(loop, ctx)
      case Some(DefineFunction(signature, body)) => ctx.putObject(RuntimeFunction.Stored(signature, body)).pure
      case Some(ExitLoop) => wrapResult(ctx.exitLoop).flatMap(interpretCode(code.tail, _))
      case Some(ExitFunction(push)) =>
        interpretCode(TPLCode(push), ctx)
          .flatMap(c => wrapResult(c.exitFunction))
          .flatMap(interpretCode(code.tail, _))
      case None => ctx.pure
    }

  private def interpretBranch(branch: Branch, ctx: InterpreterContext[F]): F[InterpreterContext[F]] = {
    // fixme: Rewrite with no recursion
    def interpretBranchRec(ctx: InterpreterContext[F], casesLeft: List[BranchCase]): F[InterpreterContext[F]] =
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

  private def interpretLoop(loop: Loop, ctx: InterpreterContext[F]): F[InterpreterContext[F]] = {
    // fixme: Rewrite with no recursion
    def interpretLoopRec(ctx: InterpreterContext[F]): F[InterpreterContext[F]] =
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
