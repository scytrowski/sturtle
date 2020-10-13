package com.github.scytrowski.sturtle.es

trait QueryHandler[F[_], S, Q <: Query[S]] {
  def handle(state: S, query: Q): F[query.Answer]
}
