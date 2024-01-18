package com.vanillasource.eliot.eliotc.feedback

import cats.effect.IO
import org.apache.logging.log4j.{LogManager, Logger}

trait Logging {
  private val log: Logger = LogManager.getLogger(this.getClass)

  def debug(msg: String): IO[Unit]               = IO.blocking(log.debug(msg))
  def debug(msg: String, t: Throwable): IO[Unit] = IO.blocking(log.debug(msg, t))
  def info(msg: String): IO[Unit]                = IO.blocking(log.info(msg))
  def info(msg: String, t: Throwable): IO[Unit]  = IO.blocking(log.info(msg, t))
  def warn(msg: String): IO[Unit]                = IO.blocking(log.warn(msg))
  def warn(msg: String, t: Throwable): IO[Unit]  = IO.blocking(log.warn(msg, t))
  def error(msg: String): IO[Unit]               = IO.blocking(log.error(msg))
  def error(msg: String, t: Throwable): IO[Unit] = IO.blocking(log.error(msg, t))
}
