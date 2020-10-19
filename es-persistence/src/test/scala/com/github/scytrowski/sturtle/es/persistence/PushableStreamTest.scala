package com.github.scytrowski.sturtle.es.persistence

import cats.effect.IO
import com.github.scytrowski.sturtle.es.persistence.fixture.EffectSpecLike

class PushableStreamTest extends EffectSpecLike {
  "PushableStream" should {

    "emit pushed items" in {
      val io = for {
        dataRef <- ref(List.empty[String])
        stream  = PushableStream.resource[IO, String, Unit](_.evalMap(e => dataRef.update(_ :+ e)))
        _ <- stream.use { push =>
          for {
            _ <- push("a")
            _ <- push("b")
            _ <- push("c")
          } yield ()
        }
        d <- dataRef.get
      } yield d

      val result = io.unsafeRunSync()
      result must contain theSameElementsInOrderAs List("a", "b", "c")
    }

  }
}
