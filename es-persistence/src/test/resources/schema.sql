DROP TABLE IF EXISTS events;
CREATE TABLE events(
    entityId VARCHAR(255) NOT NULL,
    id VARCHAR(255) NOT NULL,
    `timestamp` BIGINT NOT NULL,
    data VARCHAR(1000) NOT NULL,
    PRIMARY KEY (entityId, id)
);

DROP TABLE IF EXISTS snapshots;
CREATE TABLE snapshots(
    entityId VARCHAR(255) NOT NULL,
    id VARCHAR(255) NOT NULL,
    `timestamp` BIGINT NOT NULL,
    data VARCHAR(1000) NOT NULL,
    PRIMARY KEY (entityId, id)
);