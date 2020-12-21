package com.github.scytrowski.sturtle.tpl.module

import cats.MonadError
import cats.effect.Resource
import com.github.scytrowski.sturtle.tpl.TPLConfig

object TPLModules {
  // fixme: Load standard library
  def create[F[+_]: MonadError[*[_], Throwable]](config: TPLConfig[F], turtleId: String): Resource[F, List[Module[F]]] =
    config.refProvider
      .ref(turtleId)
      .controller
      .map { controller =>
        List(
          new SpecialFunctionsModule[F].apply(),
          new MathModule[F].apply(),
          new TurtleFunctionsModule[F].apply(controller),
        ) ++ config.customModules
      }
}
