package com.github.scytrowski.sturtle.tpl

import cats.effect.Concurrent
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.instances.list._
import com.github.scytrowski.sturtle.tpl.codegen.TPLCodeGenerator
import com.github.scytrowski.sturtle.tpl.interpreter.{InterpreterContext, TPLCode, TPLInterpreter}
import com.github.scytrowski.sturtle.tpl.lexer.TPLLexer
import com.github.scytrowski.sturtle.tpl.loader.TPLLoader
import com.github.scytrowski.sturtle.tpl.module.TPLModules
import com.github.scytrowski.sturtle.tpl.parser.TPLParser
import fs2.Stream

final class TPL[F[+_]: Concurrent] {
  def run(config: TPLConfig[F], turtleId: String, source: Stream[F, String]): F[InterpreterContext[F]] =
    for {
      code    <- generateCode(source)
      preCtx  <- TPLModules.create(config, turtleId).use(loader.loadAll[List])
      postCtx <- interpreter.interpret(code, preCtx)
    } yield postCtx

  private def generateCode(source: Stream[F, String]): F[TPLCode] =
    source
      .through(lexer.pipe)
      .compile
      .toList
      .flatMap(parser.parse(_).compile[F])
      .map(codeGen.generate)

  private val lexer = new TPLLexer[F]
  private val parser = TPLParser
  private val interpreter = new TPLInterpreter[F]
  private val codeGen = TPLCodeGenerator
  private val loader = new TPLLoader[F](interpreter)
}
