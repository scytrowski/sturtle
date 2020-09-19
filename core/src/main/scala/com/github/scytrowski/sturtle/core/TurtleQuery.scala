package com.github.scytrowski.sturtle.core

import cats.Applicative
import com.github.scytrowski.sturtle.es.{Query, QueryHandler}
import com.github.scytrowski.sturtle.geometry.{Angle, Path, Point}
import com.github.scytrowski.sturtle.graphics.Color

sealed abstract class TurtleQuery extends Query[Turtle]

object TurtleQuery {
  def handler[F[_]: Applicative]: QueryHandler[Turtle, TurtleQuery, F] =
    new QueryHandler[Turtle, TurtleQuery, F] {
      override def handle(state: Turtle, query: TurtleQuery): F[query.Answer] =
        Applicative[F].pure(query.extractAnswer(state))
    }

  case object GetPosition extends TurtleQuery {
    override type Answer = Point
    override def extractAnswer(state: Turtle): Point = state.position
  }
  case object GetAngle extends TurtleQuery {
    override type Answer = Angle
    override def extractAnswer(state: Turtle): Angle = state.angle
  }
  case object GetPath extends TurtleQuery {
    override type Answer = Path
    override def extractAnswer(state: Turtle): Path = state.path
  }
  case object GetPenState extends TurtleQuery {
    override type Answer = PenState
    override def extractAnswer(state: Turtle): PenState = state.penState
  }
  case object GetPenColor extends TurtleQuery {
    override type Answer = Color
    override def extractAnswer(state: Turtle): Color = state.penColor
  }
  case object GetFillColor extends TurtleQuery {
    override type Answer = Color
    override def extractAnswer(state: Turtle): Color = state.fillColor
  }
}
