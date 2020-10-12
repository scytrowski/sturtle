package com.github.scytrowski.sturtle.remoting.util

import java.util.UUID

import cats.Eval
import cats.effect.Sync

private[remoting] object IdGenerator {
  def uuid[F[_]: Sync]: F[String] = Sync[F].catchNonFatalEval(Eval.always(UUID.randomUUID().toString))
}
