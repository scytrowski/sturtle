package com.github.scytrowski.sturtle.tpl.fixture

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._

trait TableFixture extends TableDrivenPropertyChecks {
  protected val truthTable1 = Table("p", false, true)
  protected val truthTable2 = Table(
    ("p", "q"),
    (false, false),
    (false, true),
    (true, false),
    (true, true)
  )
}
