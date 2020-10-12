package com.github.scytrowski.sturtle.remoting

import java.net.InetSocketAddress

import cats.effect.IO
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleController, TurtleManager, TurtleQuery, TurtleQueryAnswer}
import com.github.scytrowski.sturtle.geometry.{Angle, Point}
import com.github.scytrowski.sturtle.remoting.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.remoting.server.{TurtleServer, TurtleServerHandler}

class RemotingTest extends EffectSpecLike {
  "RemotingTest" should {

    "control single turtle via TCP socket" in {
      val position = Point.cartesian(19, -7)
      val angle = Angle.radians(2.49)

      val resource =
        for {
          manager    <- TurtleManager.resource[IO](Nil)
          server     <- TurtleServer.resource[IO](new InetSocketAddress("127.0.0.1", 0), IO.pure("client"))
          _          <- new TurtleServerHandler[IO](manager.ref, 10).handle(server).background
          controller <- new RemoteTurtleRef[IO]("turtle", TurtleConnectionInfo(server.address)).controller
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
      val secondAngle = Angle.radians(0.981)

      def controlTurtle(controller: TurtleController[IO], position: Point, angle: Angle): IO[(TurtleQueryAnswer, TurtleQueryAnswer)] =
        for {
          _              <- controller.run(TurtleCommand.MoveTo(position))
          _              <- controller.run(TurtleCommand.RotateTo(angle))
          positionAnswer <- controller.execute(TurtleQuery.GetPosition)
          angleAnswer    <- controller.execute(TurtleQuery.GetAngle)
        } yield positionAnswer -> angleAnswer

      val resource =
        for {
          manager          <- TurtleManager.resource[IO](Nil)
          server           <- TurtleServer.resource[IO](new InetSocketAddress("127.0.0.1", 0), IO.pure("client"))
          _                <- new TurtleServerHandler[IO](manager.ref, 10).handle(server).background
          firstController  <- new RemoteTurtleRef[IO]("turtle-1", TurtleConnectionInfo(server.address)).controller
          secondController <- new RemoteTurtleRef[IO]("turtle-2", TurtleConnectionInfo(server.address)).controller
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

  }
}
