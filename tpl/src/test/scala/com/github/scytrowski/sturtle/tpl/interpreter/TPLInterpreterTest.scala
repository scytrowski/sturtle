package com.github.scytrowski.sturtle.tpl.interpreter

import cats.data.NonEmptyList
import com.github.scytrowski.sturtle.tpl.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.{EmptyStack, FunctionNotFound, NotInFunction, NotInLoop, VariableNotFound}
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction.{Branch, BranchCase, DefineFunction, ExitFunction, ExitLoop, Invoke, Loop, PopTo, PushFrom, PushValue}
import com.github.scytrowski.sturtle.tpl.types.Complex
import com.github.scytrowski.sturtle.tpl.types.Nat.{_1, _3}
import org.scalatest.{Inside, OptionValues}

import scala.util.{Failure, Success, Try}

class TPLInterpreterTest extends EffectSpecLike with Inside with OptionValues {
  "TPLInterpreter" should {
    "interpret PushValue" in {
      val value = NumberValue(Complex.real(1337))

      expectPush()(PushValue(value)) mustBe value
    }

    "interpret PushFrom" in {
      val signature = VariableSignature("abc")
      val value = StringValue("test123")
      val ctx = InterpreterContext
        .initial[Try]
        .putObject(RuntimeVariable(signature, value))

      expectPush(ctx)(PushFrom(signature)) mustBe value
    }

    "interpret Invoke" in {
      val signature = FunctionSignature("f", _1)
      val value = NumberValue(Complex.real(123))
      val body = TPLCode.empty.withExit(PushValue(value))
      val ctx = InterpreterContext
        .initial[Try]
        .pushValue(StringValue("abcd"))

      expectPush(ctx)(DefineFunction(signature, body), Invoke(signature))
    }

    "interpret PopTo" in {
      val signature = VariableSignature("a")
      val value = StringValue("testtest")
      val ctx = InterpreterContext
        .initial[Try]
        .pushValue(value)

      expectVariable(signature, ctx)(PopTo(signature)) mustBe value
    }

    "interpret Branch" when {
      val firstCondition = TPLCode.empty.withPush(PushFrom(VariableSignature("a")))
      val firstCase = TPLCode.empty.withPush(PushValue(StringValue("abc")))

      val secondCondition = TPLCode.empty.withPush(PushFrom(VariableSignature("b")))
      val secondCase = TPLCode.empty.withPush(PushValue(StringValue("def")))

      val branch = Branch(NonEmptyList.of(
        BranchCase(firstCondition, firstCase),
        BranchCase(secondCondition, secondCase)
      ))

      "enter first case" in {
        val ctx = InterpreterContext
          .initial[Try]
          .putObject(RuntimeVariable(VariableSignature("a"), BooleanValue(true)))

        expectPush(ctx)(branch) mustBe StringValue("abc")
      }

      "enter second case" in {
        val ctx = InterpreterContext
          .initial[Try]
          .putObject(RuntimeVariable(VariableSignature("a"), BooleanValue(false)))
          .putObject(RuntimeVariable(VariableSignature("b"), BooleanValue(true)))

        expectPush(ctx)(branch) mustBe StringValue("def")
      }

      "enter no case" in {
        val ctx = InterpreterContext
          .initial[Try]
          .putObject(RuntimeVariable(VariableSignature("a"), BooleanValue(false)))
          .putObject(RuntimeVariable(VariableSignature("b"), BooleanValue(false)))

        expectEmptyStack(ctx)(branch)
      }
    }

    "interpret Loop" when {
      "enter once" in {
        val signature = VariableSignature("a")
        val ctx = InterpreterContext
          .initial[Try]
          .putObject(RuntimeVariable(signature, BooleanValue(true)))
        val condition = TPLCode.empty.withPush(PushFrom(signature))
        val body = TPLCode(
          PushValue(NumberValue(Complex.real(1337))),
          PushValue(BooleanValue(false)),
          PopTo(signature)
        )

        expectPush(ctx)(Loop(condition, body)) mustBe NumberValue(Complex.real(1337))
      }

      "do not enter" in {
        val condition = TPLCode.empty.withPush(PushValue(BooleanValue(false)))
        val body = TPLCode(PushValue(NumberValue(Complex.real(1337))))

        expectEmptyStack()(Loop(condition, body))
      }
    }

    "interpret FunctionDefinition" in {
      val signature = FunctionSignature("f", _3)
      val body = TPLCode.empty.withExit(PushValue(StringValue("abc")))

      expectFunction(signature)(DefineFunction(signature, body)) mustBe RuntimeFunction.Stored(signature, body)
    }

    "interpret ExitLoop" in {
      val scope = Scope.root[Try]("123").onTop.withinLoop("abc")
      val ctx = InterpreterContext
        .initial[Try]
        .copy(scopeId = "abc", scope = scope)

      expectSuccess(ctx)(ExitLoop).scope.scopeType mustBe ScopeType.WithinLoop("abc")
    }

    "interpret ExitFunction" in {
      val scope = Scope.root[Try]("123").onTop.withinFunction("def")
      val returnValue = StringValue("1337")
      val ctx = InterpreterContext
        .initial[Try]
        .copy(scopeId = "def", scope = scope)

      val updatedCtx = expectSuccess(ctx)(ExitFunction(PushValue(returnValue)))

      updatedCtx.scope.scopeType mustBe ScopeType.Regular("123")
      updatedCtx.stack.pop.map(_._1).value mustBe returnValue
    }

    "fail" when {
      "variable not found" in {
        val signature = VariableSignature("a")

        expectFailure()(PushFrom(signature)) mustBe VariableNotFound(signature)
      }

      "function not found" in {
        val signature = FunctionSignature("f", _3)

        expectFailure()(Invoke(signature)) mustBe FunctionNotFound(signature)
      }

      "empty stack" in {
        expectFailure()(PopTo(VariableSignature("a"))) mustBe EmptyStack
      }

      "not in function" in {
        expectFailure()(ExitFunction(PushValue(BooleanValue(false)))) mustBe a[NotInFunction]
      }

      "not in loop" in {
        expectFailure()(ExitLoop) mustBe a[NotInLoop]
      }
    }
  }

  private def expectPush(ctx: InterpreterContext[Try] = InterpreterContext.initial)
                        (instructions: TPLInstruction*): Value =
    expectSuccess(ctx)(instructions:_*).stack.pop.map(_._1).value

  private def expectFunction(signature: FunctionSignature, ctx: InterpreterContext[Try] = InterpreterContext.initial)
                            (instructions: TPLInstruction*): RuntimeFunction[Try] =
    expectSuccess(ctx)(instructions:_*).scope.getFunction(signature).value

  private def expectVariable(signature: VariableSignature, ctx: InterpreterContext[Try] = InterpreterContext.initial)
                            (instructions: TPLInstruction*): Value =
    expectSuccess(ctx)(instructions:_*).scope.getVariable(signature).value.value

  private def expectEmptyStack(ctx: InterpreterContext[Try] = InterpreterContext.initial)(instructions: TPLInstruction*): Unit = {
    val updatedCtx = expectSuccess(ctx)(instructions:_*)
    updatedCtx.stack.pop.isDefined mustBe false
  }

  private def expectFailure(ctx: InterpreterContext[Try] = InterpreterContext.initial)
                           (instructions: TPLInstruction*): InterpreterError =
    inside(interpret(ctx)(instructions:_*)) { case Left(error) => error }

  private def expectSuccess(ctx: InterpreterContext[Try] = InterpreterContext.initial)
                       (instructions: TPLInstruction*): InterpreterContext[Try] =
    inside(interpret(ctx)(instructions:_*)) { case Right(value) => value }

  private def interpret(ctx: InterpreterContext[Try] = InterpreterContext.initial)
                       (instructions: TPLInstruction*): Either[InterpreterError, InterpreterContext[Try]] =
    inside(new TPLInterpreter[Try].interpret(TPLCode(instructions:_*), ctx)) {
      case Success(ctx) => Right(ctx)
      case Failure(InterpreterException(error)) => Left(error)
    }
}
