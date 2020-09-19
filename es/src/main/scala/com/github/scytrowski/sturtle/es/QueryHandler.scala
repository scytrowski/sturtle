package com.github.scytrowski.sturtle.es

trait QueryHandler[S, Q <: Query[S], F[_]] {
  def handle(state: S, query: Q): F[query.Answer]
}
