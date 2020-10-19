package com.github.scytrowski.sturtle.es.persistence.db

import doobie.Update
import doobie.implicits._
import doobie.util.query.Query0

object SnapshotStatement {
  def findLatest(entityId: String): Query0[SnapshotRow] =
    sql"""SELECT entityId, id, `timestamp`, data
          FROM snapshots
          WHERE entityId = $entityId
          ORDER BY `timestamp` DESC
          LIMIT 1""".query[SnapshotRow]

  val insert: Update[SnapshotRow] = {
    val sql = "INSERT INTO snapshots (entityId, id, `timestamp`, data) VALUES (?, ?, ?, ?)"
    Update[SnapshotRow](sql)
  }
}
