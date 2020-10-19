package com.github.scytrowski.sturtle.es.persistence.db

import cats.syntax.option._
import doobie.implicits._
import doobie.util.query.Query0
import doobie.{Fragments, Update}

object EventStatement {
  def findByEntityAndOptTimestamp(entityId: String, minimumTimestamp: Option[Long]): Query0[EventRow] = {
    val whereEntityIdFr = fr"entityId = $entityId".some
    val whereTimestampFr = minimumTimestamp.map(t => fr"timestamp >= $t")
    val whereFr = Fragments.whereAndOpt(whereEntityIdFr, whereTimestampFr)
    (fr"SELECT entityId, id, `timestamp`, data FROM events " ++ whereFr).query[EventRow]
  }

  val insert: Update[EventRow] = {
    val sql = "INSERT INTO events (entityId, id, `timestamp`, data) VALUES (?, ?, ?, ?)"
    Update[EventRow](sql)
  }
}
