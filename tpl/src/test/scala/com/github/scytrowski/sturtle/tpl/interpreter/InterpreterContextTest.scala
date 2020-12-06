package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.{EmptyStack, FunctionNotFound, NotInFunction, NotInLoop, VariableNotFound}
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction.PushValue
import com.github.scytrowski.sturtle.tpl.interpreter.Value.{BooleanValue, NumberValue, StringValue, VoidValue}
import org.scalatest.{Inside, OptionValues}

class InterpreterContextTest extends CommonSpecLike with Inside with OptionValues {
  "InterpreterContext" when {
    "getVariable" should {
      "succeed" in {
        val signature = VariableSignature("a")
        val value = NumberValue(1234)
        val ctx = InterpreterContext
          .initial
          .copy(scope = Scope.root.putObject(RuntimeVariable(signature, value)))

        expectSuccess(ctx.getVariable(signature)) mustBe RuntimeVariable(signature, value)
      }

      "fail" in {
        val signature = VariableSignature("a")

        expectFailure(InterpreterContext.initial.getVariable(signature)) mustBe VariableNotFound(signature)
      }
    }

    "putVariable" should {
      "succeed" in {
        val signature = VariableSignature("b")
        val value = StringValue("testtest123")

        val ctx = InterpreterContext
          .initial
          .putVariable(signature, value)

        ctx.scope.getObject(signature).value mustBe RuntimeVariable(signature, value)
      }
    }

    "getFunction" should {
      "succeed" in {
        val signature = FunctionSignature("f", 4)
        val body = TPLCode.empty.withExit(PushValue(BooleanValue(true)))
        val ctx = InterpreterContext
          .initial
          .copy(scope = Scope.root.putObject(RuntimeFunction(signature, body)))

        expectSuccess(ctx.getFunction(signature)) mustBe RuntimeFunction(signature, body)
      }

      "fail" in {
        val signature = FunctionSignature("f", 5)

        expectFailure(InterpreterContext.initial.getFunction(signature)) mustBe FunctionNotFound(signature)
      }
    }

    "putFunction" should {
      "succeed" in {
        val signature = FunctionSignature("g", 2)
        val body = TPLCode.empty.withExit(PushValue(BooleanValue(false)))

        val ctx = InterpreterContext
          .initial
          .putFunction(signature, body)

        ctx.scope.getObject(signature).value mustBe RuntimeFunction(signature, body)
      }
    }

    "pushFrom" should {
      "succeed" in {
        val signature = VariableSignature("a")
        val value = VoidValue

        val ctx = InterpreterContext
          .initial
          .putVariable(signature, value)

        expectSuccess(ctx.pushFrom(signature)).stack.pop.value._1 mustBe value
      }

      "fail" in {
        val signature = VariableSignature("a")

        expectFailure(InterpreterContext.initial.pushFrom(signature)) mustBe VariableNotFound(signature)
      }
    }

    "pop" should {
      "succeed" in {
        val value = NumberValue(4321)

        val ctx = InterpreterContext
          .initial
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
        val value = NumberValue(4321)

        val ctx = InterpreterContext
          .initial
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

        ctx.scope.scopeType mustBe ScopeType.WithinFunction
      }
    }

    "enterLoop" should {
      "succeed" in {
        val ctx = InterpreterContext
          .initial
          .enterLoop

        ctx.scope.scopeType mustBe ScopeType.WithinLoop
      }
    }

    "exitFunction" should {
      "succeed" in {
        val ctx = InterpreterContext
          .initial
          .copy(scope = Scope.root.onTop.withinFunction)

        expectSuccess(ctx.exitFunction).scope.scopeType mustBe ScopeType.Regular
      }

      "fail" in {
        expectFailure(InterpreterContext.initial.exitFunction) mustBe NotInFunction
      }
    }

    "exitLoop" should {
      "succeed" in {
        val ctx = InterpreterContext
          .initial
          .copy(scope = Scope.root.onTop.withinLoop)

        expectSuccess(ctx.exitLoop).scope.scopeType mustBe ScopeType.Regular
      }

      "fail" in {
        expectFailure(InterpreterContext.initial.exitLoop) mustBe NotInLoop
      }
    }
  }

  private def expectSuccess[A](e: Either[InterpreterError, A]): A =
    inside(e) { case Right(result) => result }

  private def expectFailure[U](e: Either[InterpreterError, U]): InterpreterError =
    inside(e) { case Left(error) => error }
}
