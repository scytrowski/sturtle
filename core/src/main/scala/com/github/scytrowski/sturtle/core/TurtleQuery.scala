package com.github.scytrowski.sturtle.core

import cats.Applicative
import com.github.scytrowski.sturtle.core.TurtleQueryAnswer._
import com.github.scytrowski.sturtle.es.{Query, QueryHandler}

sealed abstract class TurtleQuery extends Query[Turtle] {
  final override type Answer = TurtleQueryAnswer
}

object TurtleQuery {
  def handler[F[_]: Applicative]: QueryHandler[F, Turtle, TurtleQuery] =
    new QueryHandler[F, Turtle, TurtleQuery] {
      override def handle(state: Turtle, query: TurtleQuery): F[query.Answer] =
        Applicative[F].pure(query.extractAnswer(state))
    }

  case object GetPosition extends TurtleQuery {
    override def extractAnswer(state: Turtle): TurtleQueryAnswer = PositionAnswer(state.position)
  }
  case object GetAngle extends TurtleQuery {
    override def extractAnswer(state: Turtle): TurtleQueryAnswer = AngleAnswer(state.angle)
  }
  case object GetPath extends TurtleQuery {
    override def extractAnswer(state: Turtle): TurtleQueryAnswer = PathAnswer(state.path)
  }
  case object GetPenState extends TurtleQuery {
    override def extractAnswer(state: Turtle): TurtleQueryAnswer = PenStateAnswer(state.penState)
  }
  case object GetPenColor extends TurtleQuery {
    override def extractAnswer(state: Turtle): TurtleQueryAnswer = PenColorAnswer(state.penColor)
  }
  case object GetFillColor extends TurtleQuery {
    override def extractAnswer(state: Turtle): TurtleQueryAnswer = FillColorAnswer(state.fillColor)
  }
}
