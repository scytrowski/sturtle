package com.github.scytrowski.sturtle.tpl.fixture

import org.scalatest.matchers.must.Matchers

trait RandomnessFixture extends RandomValues { this: Matchers =>
  protected implicit val defaultRandomNumberParameters: RandomNumberParameters = RandomNumberParameters(-100000, 100000)
}
