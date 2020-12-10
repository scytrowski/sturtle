package com.github.scytrowski.sturtle.tpl.interpreter

import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{ApplicativeError, MonadError}
import shapeless._
import shapeless.ops.nat.ToInt

sealed abstract class RuntimeObject[+F[_]] {
  type S <: Signature

  val signature: S
}

final case class RuntimeVariable(signature: VariableSignature, value: Value) extends RuntimeObject[Nothing] {
  override type S = VariableSignature
}

sealed abstract class RuntimeFunction[F[_]] extends RuntimeObject[F] {
  override type S = FunctionSignature

  def invoke(interpreter: Interpreter[F, TPLCode], ctx: InterpreterContext[F]): F[InterpreterContext[F]]
}

object RuntimeFunction {
  def apply[F[_]: MonadError[*[_], Throwable]](signature: FunctionSignature)(implicit toInt: ToInt[signature.ParamsN]) = new RuntimeFunctionFactory[F, signature.ParamsN](signature)

  final class RuntimeFunctionFactory[F[_]: MonadError[*[_], Throwable], PN <: Nat : ToInt](val sign: FunctionSignature.Aux[PN]) {
    val void: RuntimeFunction[F] = const(VoidValue)

    def const(value: Value): RuntimeFunction[F] = pure(_ => value)

    def pure(f: PList.Aux[PN] => Value): RuntimeFunction[F] = native(f.andThen(_.pure))

    def native(f: PList.Aux[PN] => F[Value]): RuntimeFunction[F] =
      new Native[F, PN] {
        override val signature: FunctionSignature = sign

        override protected def invokeN(params: ParamsList): F[Value] = f(params)
      }
  }

  final case class Stored[F[_]](signature: FunctionSignature, code: TPLCode.WithExit) extends RuntimeFunction[F] {
    override def invoke(interpreter: Interpreter[F, TPLCode], ctx: InterpreterContext[F]): F[InterpreterContext[F]] =
      interpreter.interpret(code, ctx)
  }

  sealed abstract class Native[F[_]: MonadError[*[_], Throwable], PN <: Nat : ToInt] extends RuntimeFunction[F] {
    final type ParamsList = PList.Aux[PN]

    protected def invokeN(params: ParamsList): F[Value]

    final def invoke(interpreter: Interpreter[F, TPLCode], ctx: InterpreterContext[F]): F[InterpreterContext[F]] =
      for {
        popR   <- popParams(ctx)
        (params, updatedCtx) = popR
        result <- invokeN(params)
      } yield updatedCtx.pushValue(result)

    private def popParams(ctx: InterpreterContext[F]): F[(ParamsList, InterpreterContext[F])] = {
      // fixme: Rewrite with no recursion
      def popParamsRec(ctx: InterpreterContext[F], params: List[Value], remaining: Int): Either[InterpreterError, (ParamsList, InterpreterContext[F])] =
        if (remaining > 0)
          ctx.pop.flatMap { case (param, updatedCtx) =>
            popParamsRec(updatedCtx, param :: params, remaining - 1)
          }
        else
          Right(PList.wrap[PN](params) -> ctx)

      ApplicativeError[F, Throwable].fromEither {
        popParamsRec(ctx, List.empty, Nat.toInt[PN]).leftMap(InterpreterException)
      }
    }
  }
}
