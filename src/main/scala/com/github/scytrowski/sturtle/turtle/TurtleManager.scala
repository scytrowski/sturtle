package com.github.scytrowski.sturtle.turtle

import cats.data.NonEmptyList
import cats.effect.concurrent.Ref
import cats.effect.{Resource, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Monad}
import com.github.scytrowski.sturtle.es.{EventSourcing, EventSourcingDescription}

final class TurtleManager[F[_]: Sync] private(eventSourcing: TurtleEventSourcing[F],
                                                 controllers: Ref[F, Map[String, TurtleController[F]]]) {
  def create(id: String): Resource[F, TurtleController[F]] = {
    Resource.make(acquireController(id, Turtle.initial))(releaseController)
  }

  private def acquireController(id: String, turtle: Turtle): F[TurtleController[F]] =
    for {
      controller <- TurtleController[F](id, turtle, eventSourcing)
      _          <- controllers.update(_.updated(id, controller))
    } yield controller

  private def releaseController(controller: TurtleController[F]): F[Unit] = controllers.update(_.removed(controller.id))
}

object TurtleManager {
  def resource[F[_]: Sync](extensions: List[TurtleExtension]): Resource[F, TurtleManager[F]] =
    for {
      eventSourcing <- buildEventSourcing[F](extensions)
      controllers   <- createControllersRef[F]
    } yield new TurtleManager[F](eventSourcing, controllers)

  private def buildEventSourcing[F[_]: Monad](extensions: List[TurtleExtension]): Resource[F, TurtleEventSourcing[F]] = {
    val desc = description[F]
    val eventSourcingParts = extensions.map(_.eventSourcingFactory(desc))
    val nel = NonEmptyList.of(Resource.pure(EventSourcing.basic(desc)), eventSourcingParts:_*)
    nel
      .sequence
      .map(_.reduceLeft(_ andThen _))
  }

  private def createControllersRef[F[_]: Sync]: Resource[F, Ref[F, Map[String, TurtleController[F]]]] = {
    val ref = Ref.of[F, Map[String, TurtleController[F]]](Map.empty)
    Resource.liftF(ref)
  }

  private def description[F[_]: Applicative]: TurtleEventSourcingDescription[F] =
    EventSourcingDescription(Turtle.initial, TurtleCommand.handler[F], TurtleEvent.handler[F])
}
