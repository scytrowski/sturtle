package com.github.scytrowski.sturtle.remoting

import java.net.InetSocketAddress

import cats.effect.{IO, Resource}
import com.github.scytrowski.sturtle.core._
import com.github.scytrowski.sturtle.core.geometry.{Angle, Point}
import com.github.scytrowski.sturtle.es.{EventSourcing, EventSourcingDescription, EventSourcingSinks, EventStore, LocalEntityLockManager}
import com.github.scytrowski.sturtle.remoting.fixture.EffectSpecLike

class TurtleRemotingTest extends EffectSpecLike {
  "TurtleRemoting" should {

    "control single turtle via TCP socket" in {
      val position = Point.cartesian(19, -7)
      val angle = Angle.radians(2.49)

      val resource =
        for {
          refProvider <- Resource.liftF(testRefProvider)
          address     <- TurtleRemoting.start[IO](randomPortLocalConnectionInfo, refProvider, 1)
          controller  <- new RemoteTurtleRef[IO]("turtle", TurtleConnectionInfo(address)).controller
        } yield controller
      val io = resource.use { controller =>
        for {
          _              <- controller.run(TurtleCommand.MoveTo(position))
          _              <- controller.run(TurtleCommand.RotateTo(angle))
          positionAnswer <- controller.execute(TurtleQuery.GetPosition)
          angleAnswer    <- controller.execute(TurtleQuery.GetAngle)
        } yield positionAnswer -> angleAnswer
      }
      val (positionAnswer, angleAnswer) = io.runTimed()

      positionAnswer mustBe TurtleQueryAnswer.PositionAnswer(position)
      angleAnswer mustBe TurtleQueryAnswer.AngleAnswer(angle)
    }

    "control multiple turtles via TCP socket" in {
      val firstPosition = Point.cartesian(152, 0.987)
      val firstAngle = Angle.radians(1.23)
      val secondPosition = Point.cartesian(-1.29, 8.732)
      val secondAngle = Angle.radians(2.78)

      def controlTurtle(controller: TurtleController[IO], position: Point, angle: Angle): IO[(TurtleQueryAnswer, TurtleQueryAnswer)] =
        for {
          _              <- controller.run(TurtleCommand.MoveTo(position))
          _              <- controller.run(TurtleCommand.RotateTo(angle))
          positionAnswer <- controller.execute(TurtleQuery.GetPosition)
          angleAnswer    <- controller.execute(TurtleQuery.GetAngle)
        } yield positionAnswer -> angleAnswer

      val resource =
        for {
          refProvider      <- Resource.liftF(testRefProvider)
          address          <- TurtleRemoting.start[IO](randomPortLocalConnectionInfo, refProvider, 2)
          firstController  <- new RemoteTurtleRef[IO]("turtle-1", TurtleConnectionInfo(address)).controller
          secondController <- new RemoteTurtleRef[IO]("turtle-2", TurtleConnectionInfo(address)).controller
        } yield firstController -> secondController
      val io = resource.use { case (firstController, secondController) =>
        for {
          fiber1 <- controlTurtle(firstController, firstPosition, firstAngle).start
          fiber2 <- controlTurtle(secondController, secondPosition, secondAngle).start
          result1 <- fiber1.join
          result2 <- fiber2.join
          (firstPositionAnswer, firstAngleAnswer) = result1
          (secondPositionAnswer, secondAngleAnswer) = result2
        } yield (firstPositionAnswer, secondPositionAnswer, firstAngleAnswer, secondAngleAnswer)
      }
      val (firstPositionAnswer, secondPositionAnswer, firstAngleAnswer, secondAngleAnswer) = io.runTimed()

      firstPositionAnswer mustBe TurtleQueryAnswer.PositionAnswer(firstPosition)
      secondPositionAnswer mustBe TurtleQueryAnswer.PositionAnswer(secondPosition)
      firstAngleAnswer mustBe TurtleQueryAnswer.AngleAnswer(firstAngle)
      secondAngleAnswer mustBe TurtleQueryAnswer.AngleAnswer(secondAngle)
    }

    "control turtle via proxy" in {
      val position = Point.cartesian(19, -7)
      val angle = Angle.radians(2.49)

      val resource =
        for {
          localRefProvider  <- Resource.liftF(testRefProvider)
          address           <- TurtleRemoting.start(randomPortLocalConnectionInfo, localRefProvider, 1)
          remoteRefProvider = new RemoteTurtleRefProvider[IO](TurtleConnectionInfo(address))
          proxyAddress      <- TurtleRemoting.start(randomPortLocalConnectionInfo, remoteRefProvider, 1)
          controller        <- new RemoteTurtleRef[IO]("turtle", TurtleConnectionInfo(proxyAddress)).controller
        } yield controller
      val io = resource.use { controller =>
        for {
          _              <- controller.run(TurtleCommand.MoveTo(position))
          _              <- controller.run(TurtleCommand.RotateTo(angle))
          positionAnswer <- controller.execute(TurtleQuery.GetPosition)
          angleAnswer    <- controller.execute(TurtleQuery.GetAngle)
        } yield positionAnswer -> angleAnswer
      }
      val (positionAnswer, angleAnswer) = io.runTimed()

      positionAnswer mustBe TurtleQueryAnswer.PositionAnswer(position)
      angleAnswer mustBe TurtleQueryAnswer.AngleAnswer(angle)
    }

  }

  private def testRefProvider: IO[TurtleRefProvider[IO]] =
    testEventSourcing.map(new LocalTurtleRefProvider(_))

  private def testEventSourcing: IO[TurtleEventSourcing[IO]] =
    LocalEntityLockManager[IO, String]
      .map(new EventSourcing(testEventSourcingDescription, EventStore.dummy, EventSourcingSinks.ignore, _))

  private def testEventSourcingDescription: TurtleEventSourcingDescription[IO] =
    EventSourcingDescription(Turtle.initial, TurtleCommand.handler, TurtleEvent.handler, TurtleQuery.handler)

  private def randomPortLocalConnectionInfo: TurtleConnectionInfo =
    TurtleConnectionInfo(new InetSocketAddress("127.0.0.1", 0))
}
