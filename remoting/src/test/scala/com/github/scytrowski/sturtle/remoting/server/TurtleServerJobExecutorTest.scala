package com.github.scytrowski.sturtle.remoting.server

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.instances.list._
import cats.syntax.traverse._
import com.github.scytrowski.sturtle.core.TurtleQuery.GetAngle
import com.github.scytrowski.sturtle.core.TurtleQueryAnswer.{AngleAnswer, PenStateAnswer}
import com.github.scytrowski.sturtle.core.geometry.Angle
import com.github.scytrowski.sturtle.core.{PenState, TurtleCommand, TurtleQueryAnswer}
import com.github.scytrowski.sturtle.remoting.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.remoting.mock.{TestTurtleRef, TestTurtleServerSideClient}
import com.github.scytrowski.sturtle.remoting.protocol.TurtleClientCommand
import fs2.concurrent.Queue

class TurtleServerJobExecutorTest extends EffectSpecLike {
  "TurtleServerJobExecutor" when {

    "execute" should {

      "execute SelectTurtle properly" in {
        val io =
          for {
            turtleData <- Ref.of[IO, TestTurtleRef.Data](TestTurtleRef.Data())
            clientData <- Ref.of[IO, TestTurtleServerSideClient.Data](TestTurtleServerSideClient.Data())
            _ <- executeJobs(turtleData, clientData)(TurtleServerJob.SelectTurtle("select", "123"))
            cd <- clientData.get
          } yield cd
        val clientData = io.runTimed()

        clientData.out.loneElement mustBe TurtleClientCommand.TurtleSelected("select")
      }

      "execute ReleaseTurtle properly" when {

        "turtle is selected" in {
          val io =
            for {
              turtleData <- Ref.of[IO, TestTurtleRef.Data](TestTurtleRef.Data())
              clientData <- Ref.of[IO, TestTurtleServerSideClient.Data](TestTurtleServerSideClient.Data())
              _ <- executeJobs(turtleData, clientData)(TurtleServerJob.SelectTurtle("select", "123"), TurtleServerJob.ReleaseTurtle("release"))
              cd <- clientData.get
            } yield cd
          val clientData = io.runTimed()

          clientData.out mustBe List(TurtleClientCommand.TurtleSelected("select"), TurtleClientCommand.TurtleReleased("release"))
        }

        "otherwise" in {
          val io =
            for {
              turtleData <- Ref.of[IO, TestTurtleRef.Data](TestTurtleRef.Data())
              clientData <- Ref.of[IO, TestTurtleServerSideClient.Data](TestTurtleServerSideClient.Data())
              _ <- executeJobs(turtleData, clientData)(TurtleServerJob.ReleaseTurtle("release"))
              cd <- clientData.get
            } yield cd
          val clientData = io.runTimed()

          clientData.out.loneElement mustBe TurtleClientCommand.NoTurtleSelected("release")
        }

      }

      "execute RunCommand properly" when {

        "turtle is selected" in {
          val command = TurtleCommand.PenUp

          val io =
            for {
              turtleData <- Ref.of[IO, TestTurtleRef.Data](TestTurtleRef.Data())
              clientData <- Ref.of[IO, TestTurtleServerSideClient.Data](TestTurtleServerSideClient.Data())
              _ <- executeJobs(turtleData, clientData)(TurtleServerJob.SelectTurtle("select", "123"), TurtleServerJob.RunCommand("run", command))
              td <- turtleData.get
              cd <- clientData.get
            } yield td -> cd
          val (turtleData, clientData) = io.runTimed()

          turtleData.commands.loneElement mustBe command
          clientData.out mustBe List(TurtleClientCommand.TurtleSelected("select"), TurtleClientCommand.CommandRan("run"))
        }

        "otherwise" in {
          val io =
            for {
              turtleData <- Ref.of[IO, TestTurtleRef.Data](TestTurtleRef.Data())
              clientData <- Ref.of[IO, TestTurtleServerSideClient.Data](TestTurtleServerSideClient.Data())
              _ <- executeJobs(turtleData, clientData)(TurtleServerJob.RunCommand("run", TurtleCommand.PenUp))
              td <- turtleData.get
              cd <- clientData.get
            } yield td -> cd
          val (turtleData, clientData) = io.runTimed()

          turtleData.commands.isEmpty mustBe true
          clientData.out mustBe List(TurtleClientCommand.NoTurtleSelected("run"))
        }

      }

      "execute ExecuteQuery properly" when {

        "turtle is selected" in {
          val query = GetAngle
          val answer = AngleAnswer(Angle.radians(1.23))

          val io =
            for {
              turtleData <- Ref.of[IO, TestTurtleRef.Data](TestTurtleRef.Data())
              clientData <- Ref.of[IO, TestTurtleServerSideClient.Data](TestTurtleServerSideClient.Data())
              _ <- executeJobs(turtleData, clientData, answer)(TurtleServerJob.SelectTurtle("select", "123"), TurtleServerJob.ExecuteQuery("execute", query))
              td <- turtleData.get
              cd <- clientData.get
            } yield td -> cd
          val (turtleData, clientData) = io.runTimed()

          turtleData.queries.loneElement mustBe query
          clientData.out mustBe List(TurtleClientCommand.TurtleSelected("select"), TurtleClientCommand.QueryExecuted("execute", answer))
        }

        "otherwise" in {
          val io =
            for {
              turtleData <- Ref.of[IO, TestTurtleRef.Data](TestTurtleRef.Data())
              clientData <- Ref.of[IO, TestTurtleServerSideClient.Data](TestTurtleServerSideClient.Data())
              _ <- executeJobs(turtleData, clientData)(TurtleServerJob.ExecuteQuery("execute", GetAngle))
              td <- turtleData.get
              cd <- clientData.get
            } yield td -> cd
          val (turtleData, clientData) = io.runTimed()

          turtleData.queries.isEmpty mustBe true
          clientData.out mustBe List(TurtleClientCommand.NoTurtleSelected("execute"))
        }

      }

    }

  }

  private def executeJobs[A](turtleData: Ref[IO, TestTurtleRef.Data],
                             clientData: Ref[IO, TestTurtleServerSideClient.Data],
                             answer: TurtleQueryAnswer = PenStateAnswer(PenState.Down))
                            (jobs: TurtleServerJob*): IO[Unit] =
    for {
      jobQueue <- Queue.unbounded[IO, TurtleServerJob]
      turtleRef = new TestTurtleRef("test-turtle", turtleData, answer)
      client = new TestTurtleServerSideClient(clientData)
      executor = new TurtleServerJobExecutor[IO](_ => turtleRef, client, jobQueue)
      fiber <- executor.execute.start
      _     <- (jobs :+ TurtleServerJob.Finish("finish")).toList.map(jobQueue.enqueue1).sequence
      _     <- fiber.join
    } yield ()
}
