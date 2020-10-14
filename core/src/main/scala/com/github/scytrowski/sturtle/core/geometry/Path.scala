package com.github.scytrowski.sturtle.core.geometry

final case class Path private(points: List[Point]) {
  def to(point: Point): Path = copy(points = points :+ point)
  def ~>(point: Point): Path = to(point)
}

object Path {
  val empty: Path = Path(List.empty)
}
