package com.github.scytrowski.sturtle.logging

import cats.Id
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleEvent, TurtleQuery}
import com.github.scytrowski.sturtle.geometry.{Angle, Point}
import com.github.scytrowski.sturtle.logging.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.logging.mock.TestLogger

class Log4CatsTurtleLoggerTest extends CommonSpecLike {
  "Log4CatsTurtleLogger" when {

    "logCommand" should {

      "use log4cats logger" when {

        "log level is ERROR" in {
          testCommandLogging(LogLevel.Error).error.length mustBe 1
        }

        "log level is WARN" in {
          testCommandLogging(LogLevel.Warn).warn.length mustBe 1
        }

        "log level is INFO" in {
          testCommandLogging(LogLevel.Info).info.length mustBe 1
        }

        "log level is DEBUG" in {
          testCommandLogging(LogLevel.Debug).debug.length mustBe 1
        }

        "log level is TRACE" in {
          testCommandLogging(LogLevel.Trace).trace.length mustBe 1
        }

        "logging is disabled" in {
          testCommandLogging(LogLevel.Disabled) mustBe TestLogger.TestData()
        }

      }

    }

    "logEvents" should {

      "use log4cats logger" when {

        "log level is ERROR" in {
          testEventLogging(LogLevel.Error).error.length mustBe 1
        }

        "log level is WARN" in {
          testEventLogging(LogLevel.Warn).warn.length mustBe 1
        }

        "log level is INFO" in {
          testEventLogging(LogLevel.Info).info.length mustBe 1
        }

        "log level is DEBUG" in {
          testEventLogging(LogLevel.Debug).debug.length mustBe 1
        }

        "log level is TRACE" in {
          testEventLogging(LogLevel.Trace).trace.length mustBe 1
        }

        "logging is disabled" in {
          testEventLogging(LogLevel.Disabled) mustBe TestLogger.TestData()
        }

      }

    }

    "logQuery" should {

      "use log4cats logger" when {


        "log level is ERROR" in {
          testQueryLogging(LogLevel.Error).error.length mustBe 1
        }

        "log level is WARN" in {
          testQueryLogging(LogLevel.Warn).warn.length mustBe 1
        }

        "log level is INFO" in {
          testQueryLogging(LogLevel.Info).info.length mustBe 1
        }

        "log level is DEBUG" in {
          testQueryLogging(LogLevel.Debug).debug.length mustBe 1
        }

        "log level is TRACE" in {
          testQueryLogging(LogLevel.Trace).trace.length mustBe 1
        }

        "logging is disabled" in {
          testQueryLogging(LogLevel.Disabled) mustBe TestLogger.TestData()
        }

      }

    }

  }

  private val command = TurtleCommand.MoveTo(Point.cartesian(-7, 18))
  private val event = TurtleEvent.RotatedBy(Angle.radians(1.23))
  private val query = TurtleQuery.GetPenState

  private def testCommandLogging(logLevel: LogLevel): TestLogger.TestData =
    test(commandLogLevel = logLevel)(_.logCommand(command))

  private def testEventLogging(logLevel: LogLevel): TestLogger.TestData =
    test(eventLogLevel = logLevel)(_.logEvents(List(event)))

  private def testQueryLogging(logLevel: LogLevel): TestLogger.TestData =
    test(queryLogLevel = logLevel)(_.logQuery(query))

  private def test[U](commandLogLevel: LogLevel = LogLevel.Debug,
                      eventLogLevel: LogLevel = LogLevel.Debug,
                      queryLogLevel: LogLevel = LogLevel.Debug)(f: Log4CatsTurtleLogger[Id] => Id[U]): TestLogger.TestData =
    for {
      data   <- Ref.of(TestLogger.TestData())
      logger = new TestLogger(data)
      turtleLogger = new Log4CatsTurtleLogger[Id](logger, LoggingConfig(commandLogLevel, eventLogLevel, queryLogLevel))
      _ <- f(turtleLogger)
      d <- data.get
    } yield d
}
