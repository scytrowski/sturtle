package com.github.scytrowski.sturtle.tpl.util

import java.util.UUID

object IdGenerator {
  def generate: String = uuid

  private def uuid: String = UUID.randomUUID().toString
}
