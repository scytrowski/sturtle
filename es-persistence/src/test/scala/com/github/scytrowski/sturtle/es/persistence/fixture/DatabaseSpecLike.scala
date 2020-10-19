package com.github.scytrowski.sturtle.es.persistence.fixture

import java.util.UUID

import cats.effect.{Blocker, IO, Resource}
import doobie.{ExecutionContexts, Transactor}
import doobie.h2.H2Transactor

trait DatabaseSpecLike extends EffectSpecLike {
  def useTransactor[A](f: Transactor[IO] => IO[A]): IO[A] =
    transactor.use(f)

  private def transactor: Resource[IO, Transactor[IO]] = {
    H2Transactor.newH2Transactor(
      url = generateDatabaseUrl,
      user = "sa",
      pass = "",
      connectEC = ExecutionContexts.synchronous,
      blocker = Blocker.liftExecutionContext(ExecutionContexts.synchronous)
    )
  }

  private def generateDatabaseUrl: String = {
    val databaseName = s"test_${UUID.randomUUID().toString}"
    s"jdbc:h2:mem:$databaseName;MODE=MySQL;DB_CLOSE_DELAY=-1;DATABASE_TO_UPPER=false;DB_CLOSE_ON_EXIT=FALSE;INIT=RUNSCRIPT FROM 'classpath:schema.sql'"
  }
}
