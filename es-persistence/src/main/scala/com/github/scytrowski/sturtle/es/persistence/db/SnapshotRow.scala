package com.github.scytrowski.sturtle.es.persistence.db

final case class SnapshotRow(entityId: String,
                             id: String,
                             timestamp: Long,
                             data: String)
