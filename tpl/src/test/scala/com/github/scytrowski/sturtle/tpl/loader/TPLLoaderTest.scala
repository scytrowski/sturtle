package com.github.scytrowski.sturtle.tpl.loader

import cats.effect.IO
import com.github.scytrowski.sturtle.tpl.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter._
import com.github.scytrowski.sturtle.tpl.module.Module
import org.scalatest.OptionValues
import org.scalatest.prop.TableDrivenPropertyChecks
import shapeless.Nat._5

class TPLLoaderTest extends EffectSpecLike with TableDrivenPropertyChecks with OptionValues {
  "TPLLoader" when {
    "load" when {
      "raw module" in {
        val ctx = InterpreterContext
          .initial[IO]
          .putObject(RuntimeVariable(VariableSignature("b"), NumberValue(1337)))
        val module = Module.Raw(TPLCode.empty.append(TPLInstruction.ExitLoop))

        load(module, ctx) mustBe ctx
      }

      "prepared module" in {
        val module = Module.Prepared(List(
          RuntimeVariable(VariableSignature("a"), StringValue("123")),
          RuntimeFunction[IO](FunctionSignature("f", _5)).const(StringValue("456"))
        ))

        val ctx = load(module)
        forAll(Table("obj", module.objects:_*)) { obj =>
          val actual = ctx.scope.getObject(obj.signature).value

          actual mustBe obj
        }
      }
    }
  }

  private def load(module: Module[IO], result: InterpreterContext[IO] = InterpreterContext.initial): InterpreterContext[IO] =
    new TPLLoader[IO](testInterpreter(result))
      .load(module)
      .unsafeRunSync()

  private def testInterpreter(result: InterpreterContext[IO]): Interpreter[IO, TPLCode] = (_, _) => IO.pure(result)
}
