package com.vanillasource.eliot.eliotc.feedback

import cats.effect.{IO, Sync}
import org.apache.logging.log4j.{LogManager, Logger}

trait Logging {
  private val log: Logger = LogManager.getLogger(this.getClass)

  def debug[F[_]: Sync](msg: String): F[Unit]               = Sync[F].blocking(log.debug(msg))
  def debug[F[_]: Sync](msg: String, t: Throwable): F[Unit] = Sync[F].blocking(log.debug(msg, t))
  def info[F[_]: Sync](msg: String): F[Unit]                = Sync[F].blocking(log.info(msg))
  def info[F[_]: Sync](msg: String, t: Throwable): F[Unit]  = Sync[F].blocking(log.info(msg, t))
  def warn[F[_]: Sync](msg: String): F[Unit]                = Sync[F].blocking(log.warn(msg))
  def warn[F[_]: Sync](msg: String, t: Throwable): F[Unit]  = Sync[F].blocking(log.warn(msg, t))
  def error[F[_]: Sync](msg: String): F[Unit]               = Sync[F].blocking(log.error(msg))
  def error[F[_]: Sync](msg: String, t: Throwable): F[Unit] = Sync[F].blocking(log.error(msg, t))
}
