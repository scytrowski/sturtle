package com.github.scytrowski.sturtle.logging.mock

import cats.Id
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.logging.mock.TestLogger.TestData
import io.chrisdavenport.log4cats.Logger

final class TestLogger(data: Ref[Id, TestData]) extends Logger[Id] {
  override def error(message: => String): Id[Unit] =
    data.update(d => d.copy(error = d.error :+ message))

  override def warn(message: => String): Id[Unit] =
    data.update(d => d.copy(warn = d.warn :+ message))

  override def info(message: => String): Id[Unit] =
    data.update(d => d.copy(info = d.info :+ message))

  override def debug(message: => String): Id[Unit] =
    data.update(d => d.copy(debug = d.debug :+ message))

  override def trace(message: => String): Id[Unit] =
    data.update(d => d.copy(trace = d.trace :+ message))

  override def error(t: Throwable)(message: => String): Id[Unit] =
    data.update(d => d.copy(error = d.error :+ message))

  override def warn(t: Throwable)(message: => String): Id[Unit] =
    data.update(d => d.copy(warn = d.warn :+ message))

  override def info(t: Throwable)(message: => String): Id[Unit] =
    data.update(d => d.copy(info = d.info :+ message))

  override def debug(t: Throwable)(message: => String): Id[Unit] =
    data.update(d => d.copy(debug = d.debug :+ message))

  override def trace(t: Throwable)(message: => String): Id[Unit] =
    data.update(d => d.copy(trace = d.trace :+ message))
}

object TestLogger {
  final case class TestData(error: List[String] = Nil,
                            warn: List[String] = Nil,
                            info: List[String] = Nil,
                            debug: List[String] = Nil,
                            trace: List[String] = Nil)
}
