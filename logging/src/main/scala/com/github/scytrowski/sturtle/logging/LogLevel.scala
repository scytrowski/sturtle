package com.github.scytrowski.sturtle.logging

sealed abstract class LogLevel

object LogLevel {
  case object Error extends LogLevel
  case object Warn extends LogLevel
  case object Info extends LogLevel
  case object Debug extends LogLevel
  case object Trace extends LogLevel
  case object Disabled extends LogLevel
}