package com.github.scytrowski.sturtle.core

import java.util.concurrent.Executors

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, Fiber, IO, Resource, Timer}
import cats.syntax.flatMap._
import com.github.scytrowski.sturtle.core.fixture.CommonSpecLike

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class TurtleManagerTest extends CommonSpecLike {
  "TurtleManager" should {

    "prevent usage of the same turtle by concurrent consumers" in {
      val actions = useManager { use =>
        for {
          fiber1 <- use("1", 500.millis)
          fiber2 <- use("1", 0.seconds)
          _ <- fiber1.join
          _ <- fiber2.join
        } yield ()
      }.unsafeRunSync()

      actions mustBe List(
        LockAction.Acquired("1"),
        LockAction.Released("1"),
        LockAction.Acquired("1"),
        LockAction.Released("1")
      )
    }

  }

  private def useManager(useF: ((String, FiniteDuration) => IO[Fiber[IO, Unit]]) => IO[Unit]): IO[List[LockAction]] = {
    val res =
      for {
        manager <- TurtleManager.resource[IO](List.empty)
        data    <- Resource.liftF(Ref.of[IO, List[LockAction]](List.empty))
      } yield manager -> data
    res.use { case (manager, data) =>
      val use = useTurtle(manager, data) _
      useF(use) >> data.get
    }
  }

  private def useTurtle(manager: TurtleManager[IO], data: Ref[IO, List[LockAction]])(id: String, sleepDuration: FiniteDuration): IO[Fiber[IO, Unit]] =
    manager.ref(id).flatMap { ref =>
      ref.controller.use { _ =>
        data.update(_ :+ LockAction.Acquired(id)) *> IO.sleep(sleepDuration) *> data.update(_ :+ LockAction.Released(id))
      }
    }.start

  private sealed class LockAction

  private object LockAction {
    final case class Acquired(id: String) extends LockAction
    final case class Released(id: String) extends LockAction
  }

  private val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))
  private implicit val timerIO: Timer[IO] = IO.timer(ec)
  private implicit val csIO: ContextShift[IO] = IO.contextShift(ec)
}
