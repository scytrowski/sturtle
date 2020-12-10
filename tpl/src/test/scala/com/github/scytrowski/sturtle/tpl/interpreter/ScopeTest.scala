package com.github.scytrowski.sturtle.tpl.interpreter

import cats.Id
import cats.effect.IO
import com.github.scytrowski.sturtle.tpl.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction.PushValue
import org.scalatest.{Inside, OptionValues}
import shapeless.Nat.{_0, _3, _6}

class ScopeTest extends CommonSpecLike with Inside with OptionValues {
  "Scope" should {
    "putObject" when {
      "object is variable" in {
        val variable = RuntimeVariable(VariableSignature("a"), StringValue("1337"))

        requireLayeredScope(Scope.root.putObject(variable)).objects mustBe Map(variable.signature -> variable)
      }

      "object is function" in {
        val function = RuntimeFunction.Stored(
          FunctionSignature("f", _3),
          TPLCode.empty.withExit(PushValue(VoidValue))
        )

        requireLayeredScope(Scope.root.putObject(function)).objects mustBe Map(function.signature -> function)
      }
    }

    "getObject" when {
      "signature points to variable" in {
        val variable = RuntimeVariable(VariableSignature("b"), NumberValue(1337))
        val scope = LayeredScope(
          ScopeType.Regular,
          Map(variable.signature -> variable),
          None
        )

        scope.getObject(variable.signature).value mustBe variable
      }

      "signature points to function" in {
        val function = RuntimeFunction.Stored(
          FunctionSignature("g", _6),
          TPLCode.empty.withExit(PushValue(VoidValue))
        )
        val scope = LayeredScope(
          ScopeType.Regular,
          Map(function.signature -> function),
          None
        )

        scope.getObject(function.signature).value mustBe function
      }
    }

    "merge" should {
      "use objects from outer scope as a first priority" in {
        val varSignature = VariableSignature("a")
        val funcSignature = FunctionSignature("f", _0)
        val expectedValue = StringValue("expected value")
        val expectedFunction = RuntimeFunction[IO](funcSignature).const(StringValue("expectedFunction"))
        val scope1 = Scope.root
          .putObject(RuntimeVariable(varSignature, StringValue("unexpected value")))
          .putObject(RuntimeFunction[IO](funcSignature).const(StringValue("unexpectedFunction")))
        val scope2 = Scope.root
          .putObject(RuntimeVariable(varSignature, expectedValue))
          .putObject(expectedFunction)

        val merged = scope1.merge(scope2)

        val actualValue = merged.getVariable(varSignature).value
        val actualFunction = merged.getFunction(funcSignature).value
        actualValue mustBe RuntimeVariable(varSignature, expectedValue)
        actualFunction mustBe expectedFunction
      }
    }

    "withinFunction" in {
      Scope.root.withinFunction.scopeType mustBe ScopeType.WithinFunction
    }

    "withinLoop" in {
      Scope.root.withinLoop.scopeType mustBe ScopeType.WithinLoop
    }

    "parent" when {
      "fallback scope is defined" in {
        val parent = Scope.root[Id]
        val child = LayeredScope(
          ScopeType.WithinFunction,
          Map.empty,
          Some(parent)
        )

        child.parent mustBe parent
      }

      "otherwise" in {
        val altRoot = LayeredScope(
          ScopeType.WithinLoop,
          Map.empty,
          None
        )

        altRoot.parent mustBe altRoot
      }
    }

    "onTop" in {
      val someVariable = RuntimeVariable(VariableSignature("c"), BooleanValue(false))
      val scope = LayeredScope[Id](
        ScopeType.WithinFunction,
        Map(someVariable.signature -> someVariable),
        None
      )

      scope.onTop mustBe scope.copy(
        objects = Map.empty,
        fallbackScope = Some(scope)
      )
    }
  }

  private def requireLayeredScope(scope: Scope[Id]): LayeredScope[Id] =
    inside(scope) { case layered: LayeredScope[Id] => layered }
}
