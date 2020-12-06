package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction.PushValue
import com.github.scytrowski.sturtle.tpl.interpreter.Value.{BooleanValue, NumberValue, StringValue, VoidValue}
import org.scalatest.{Inside, OptionValues}

class ScopeTest extends CommonSpecLike with Inside with OptionValues {
  "Scope" should {
    "putObject" when {
      "object is variable" in {
        val variable = RuntimeVariable(VariableSignature("a"), StringValue("1337"))

        requireLayeredScope(Scope.root.putObject(variable)).objects mustBe Map(variable.signature -> variable)
      }

      "object is function" in {
        val function = RuntimeFunction(
          FunctionSignature("f", 3),
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
        val function = RuntimeFunction(FunctionSignature("g", 6), TPLCode.empty.withExit(PushValue(VoidValue)))
        val scope = LayeredScope(
          ScopeType.Regular,
          Map(function.signature -> function),
          None
        )

        scope.getObject(function.signature).value mustBe function
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
        val parent = Scope.root
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
      val scope = LayeredScope(
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

  private def requireLayeredScope(scope: Scope): LayeredScope =
    inside(scope) { case layered: LayeredScope => layered }
}
