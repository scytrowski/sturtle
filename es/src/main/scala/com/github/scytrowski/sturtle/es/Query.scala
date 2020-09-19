package com.github.scytrowski.sturtle.es

trait Query[S] {
  type Answer
  def extractAnswer(state: S): Answer
}
