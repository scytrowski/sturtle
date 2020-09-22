package com.github.scytrowski.sturtle.logging

final case class LoggingConfig(commandLogLevel: LogLevel,
                               eventLogLevel: LogLevel,
                               queryLogLevel: LogLevel)
