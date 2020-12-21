package com.github.scytrowski.sturtle.tpl.module

import cats.MonadError
import cats.data.OptionT
import cats.syntax.functor._
import cats.syntax.flatMap._
import com.github.scytrowski.sturtle.tpl.interpreter.{InterpreterError, NumberValue, RuntimeFunction, RuntimeVariable}
import com.github.scytrowski.sturtle.tpl.types.Complex
import com.github.scytrowski.sturtle.tpl.types.Nat.{_0, _1}
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.InternalError

final class MathModule[F[+_]: MonadError[*[_], Throwable]] extends NativeModuleOps[F] {
  def apply(): Module[F] =
    Module.Prepared(List(
      pi,
      e,
      i,
      re,
      im,
      con,
      abs,
      arg,
      floor,
      ceil,
      min,
      max,
      pow,
      exp,
      log,
      ln,
      sin,
      cos
    ))

  private val pi = RuntimeVariable(MathConstants.pi, NumberValue(Complex.real(Math.PI)))

  private val e = RuntimeVariable(MathConstants.e, NumberValue(Complex.real(Math.E)))

  private val i = RuntimeVariable(MathConstants.i, NumberValue(Complex.unit))

  private val re = RuntimeFunction(MathFunctions.re).native(unaryNumberFunction { z =>
    NumberValue(Complex.real(z.real))
  })

  private val im = RuntimeFunction(MathFunctions.im).native(unaryNumberFunction { z =>
    NumberValue(Complex.real(z.imaginary))
  })

  private val con = RuntimeFunction(MathFunctions.con).native(unaryNumberFunction { z =>
    NumberValue(z.conjugate)
  })

  private val abs = RuntimeFunction(MathFunctions.abs).native(unaryNumberFunction { z =>
    NumberValue(Complex.real(z.abs))
  })

  private val arg = RuntimeFunction(MathFunctions.arg).native(unaryNumberFunction { z =>
    NumberValue(Complex.real(z.arg))
  })

  private val floor = RuntimeFunction(MathFunctions.floor).native(unaryRealFunction { r =>
    NumberValue(Complex.real(Math.floor(r)))
  })

  private val ceil = RuntimeFunction(MathFunctions.ceil).native(unaryRealFunction { r =>
    NumberValue(Complex.real(Math.ceil(r)))
  })

  private val min = RuntimeFunction(MathFunctions.min).native(binaryRealFunction { case (l, r) =>
    NumberValue(Complex.real(Math.min(l, r)))
  })

  private val max = RuntimeFunction(MathFunctions.max).native(binaryRealFunction { case (l, r) =>
    NumberValue(Complex.real(Math.max(l, r)))
  })

  private val pow = RuntimeFunction(MathFunctions.pow).native(binaryNumberFunction { case (l, r) =>
    NumberValue(l ** r)
  })

  private val exp = RuntimeFunction(MathFunctions.exp).native(unaryNumberFunction { z =>
    NumberValue(z.exp)
  })

  private val log = RuntimeFunction(MathFunctions.log).native { params =>
    for {
      a    <- wrap(params.requireNonZero(_0))
      base <- wrap(params.requireValidLogBase(_1))
      res  <- OptionT.fromOption[F](a.log(base)).getOrElseF(raiseError(logCannotBeComputed(a, base)))
    } yield NumberValue(res)
  }

  private val ln = RuntimeFunction(MathFunctions.ln).native { params =>
    for {
      a    <- wrap(params.requireNonZero(_0))
      res  <- OptionT.fromOption[F](a.ln).getOrElseF(raiseError(logCannotBeComputed(a, Complex.real(Math.E))))
    } yield NumberValue(res)
  }

  private val sin = RuntimeFunction(MathFunctions.sin).native(unaryNumberFunction { v =>
    NumberValue(v.sin)
  })

  private val cos = RuntimeFunction(MathFunctions.cos).native(unaryNumberFunction { v =>
    NumberValue(v.cos)
  })

  private def logCannotBeComputed(a: Complex, base: Complex): InterpreterError =
    InternalError(new IllegalStateException(s"log($a, $base) is not defined"))
}
