package com.github.scytrowski.sturtle.es

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.es.fixture.EffectSpecLike
import scala.concurrent.duration._

class LocalEntityLockManagerTest extends EffectSpecLike {
  "LockEntityLockManager" when {

    "retrieve" should {

      "return shared lock" in {
        def consumer1(dataRef: Ref[IO, List[String]]) =
          for {
            _ <- IO.sleep(500.millis)
            _ <- dataRef.update(_ :+ "a")
          } yield ()

        def consumer2(dataRef: Ref[IO, List[String]]) =
          dataRef.update(_ :+ "b")

        val io =
          for {
            dataRef     <- ref(List.empty[String])
            lockManager <- LocalEntityLockManager[IO, String]
            fiber1      <- lockManager.retrieve("test-entity").use(_ => consumer1(dataRef)).start
            _           <- IO.sleep(100.millis)
            fiber2      <- lockManager.retrieve("test-entity").use(_ => consumer2(dataRef)).start
            _           <- fiber1.join
            _           <- fiber2.join
            data        <- dataRef.get
          } yield data

        val data = io.unsafeRunSync()
        data must contain theSameElementsInOrderAs List("a", "b")
      }

    }

  }
}
