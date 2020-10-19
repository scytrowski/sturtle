package com.github.scytrowski.sturtle.es.persistence.db

final case class EventRow(entityId: String,
                          id: String,
                          timestamp: Long,
                          data: String)
