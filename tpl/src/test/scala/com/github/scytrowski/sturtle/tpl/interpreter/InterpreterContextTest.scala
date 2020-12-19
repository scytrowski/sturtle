package com.github.scytrowski.sturtle.tpl.interpreter

import cats.Id
import com.github.scytrowski.sturtle.tpl.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.{EmptyStack, FunctionNotFound, NotInFunction, NotInLoop, VariableNotFound}
import com.github.scytrowski.sturtle.tpl.interpreter.ScopeType.Regular
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction.PushValue
import com.github.scytrowski.sturtle.tpl.types.{Complex, Nat}
import org.scalatest.{Inside, OptionValues}

class InterpreterContextTest extends CommonSpecLike with Inside with OptionValues {
  import Nat._

  "InterpreterContext" when {
    "getVariable" should {
      "succeed" in {
        val signature = VariableSignature("a")
        val value = NumberValue(Complex.real(1234))
        val ctx = InterpreterContext
          .initial
          .copy(scope = Scope.root("abc").putObject(RuntimeVariable(signature, value)))

        expectSuccess(ctx.getVariable(signature)) mustBe RuntimeVariable(signature, value)
      }

      "fail" in {
        val signature = VariableSignature("a")

        expectFailure(InterpreterContext.initial.getVariable(signature)) mustBe VariableNotFound(signature)
      }
    }

    "getFunction" should {
      "succeed" in {
        val signature = FunctionSignature("f", _4)
        val body = TPLCode.empty.withExit(PushValue(BooleanValue(true)))
        val ctx = InterpreterContext
          .initial
          .copy(scope = Scope.root("abc").putObject(RuntimeFunction.Stored[Id](signature, body)))

        expectSuccess(ctx.getFunction(signature)) mustBe RuntimeFunction.Stored[Id](signature, body)
      }

      "fail" in {
        val signature = FunctionSignature("f", _5)

        expectFailure(InterpreterContext.initial.getFunction(signature)) mustBe FunctionNotFound(signature)
      }
    }

    "putObject" should {
      "put variable" in {
        val signature = VariableSignature("b")
        val value = StringValue("testtest123")

        val ctx = InterpreterContext
          .initial
          .putObject(RuntimeVariable(signature, value))

        ctx.scope.getObject(signature).value mustBe RuntimeVariable(signature, value)
      }

      "put function" in {
        val signature = FunctionSignature("g", _2)
        val body = TPLCode.empty.withExit(PushValue(BooleanValue(false)))

        val ctx = InterpreterContext
          .initial
          .putObject(RuntimeFunction.Stored[Id](signature, body))

        ctx.scope.getObject(signature).value mustBe RuntimeFunction.Stored[Id](signature, body)
      }
    }

    "pushFrom" should {
      "succeed" in {
        val signature = VariableSignature("a")
        val value = VoidValue

        val ctx = InterpreterContext
          .initial
          .putObject(RuntimeVariable(signature, value))

        expectSuccess(ctx.pushFrom(signature)).stack.pop.value._1 mustBe value
      }

      "fail" in {
        val signature = VariableSignature("a")

        expectFailure(InterpreterContext.initial.pushFrom(signature)) mustBe VariableNotFound(signature)
      }
    }

    "pop" should {
      "succeed" in {
        val value = NumberValue(Complex.real(4321))

        val ctx = InterpreterContext
          .initial[Id]
          .copy(stack = Stack.empty.push(value))

        val (top, updatedCtx) = expectSuccess(ctx.pop)

        top mustBe value
        updatedCtx.stack mustBe Stack.empty
      }

      "fail" in {
        expectFailure(InterpreterContext.initial.pop) mustBe EmptyStack
      }
    }

    "popTo" should {
      "succeed" in {
        val signature = VariableSignature("a")
        val value = NumberValue(Complex.real(4321))

        val ctx = InterpreterContext
          .initial[Id]
          .copy(stack = Stack.empty.push(value))

        val updatedCtx = expectSuccess(ctx.popTo(signature))

        updatedCtx.scope.getObject(signature).value mustBe RuntimeVariable(signature, value)
        updatedCtx.stack mustBe Stack.empty
      }

      "fail" in {
        expectFailure(InterpreterContext.initial.popTo(VariableSignature("a"))) mustBe EmptyStack
      }
    }

    "enterFunction" should {
      "succeed" in {
        val ctx = InterpreterContext
          .initial
          .enterFunction

        ctx.scope.scopeType mustBe ScopeType.WithinFunction(ctx.scopeId)
      }
    }

    "enterLoop" should {
      "succeed" in {
        val ctx = InterpreterContext
          .initial
          .enterLoop

        ctx.scope.scopeType mustBe ScopeType.WithinLoop(ctx.scopeId)
      }
    }

    "exitFunction" should {
      "succeed" in {
        val ctx = InterpreterContext
          .initial
          .copy(scopeId = "123", scope = Scope.root("abc").onTop.withinFunction("123"))

        expectSuccess(ctx.exitFunction).scope.scopeType mustBe ScopeType.Regular("abc")
      }

      "fail" in {
        val ctx = InterpreterContext
          .initial
          .copy(scopeId = "123", scope = Scope.root("abc").onTop.withType(Regular("123")))

        expectFailure(ctx.exitFunction) mustBe NotInFunction(Regular("123"))
      }
    }

    "exitLoop" should {
      "succeed" in {
        val ctx = InterpreterContext
          .initial
          .copy(scopeId = "456", scope = Scope.root("abc").onTop.withinLoop("456"))

        expectSuccess(ctx.exitLoop).scope.scopeType mustBe ScopeType.WithinLoop("456")
      }

      "fail" in {
        val ctx = InterpreterContext
          .initial
          .copy(scopeId = "456", scope = Scope.root("abc").onTop.withType(Regular("456")))

        expectFailure(ctx.exitLoop) mustBe NotInLoop(Regular("456"))
      }
    }
  }

  private def expectSuccess[A](e: Either[InterpreterError, A]): A =
    inside(e) { case Right(result) => result }

  private def expectFailure[U](e: Either[InterpreterError, U]): InterpreterError =
    inside(e) { case Left(error) => error }
}
