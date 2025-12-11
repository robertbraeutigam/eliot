package com.vanillasource.eliot.eliotc.feedback

import cats.effect.Sync
import com.vanillasource.eliot.eliotc.feedback.Logging.log
import org.apache.logging.log4j.{Level, LogManager, Logger}

trait Logging {
  private val logger: Logger = LogManager.getLogger(this.getClass)

  def debug[F[_]: Sync](msg: String): F[Unit]               = log[F](logger, Level.DEBUG, msg, null)
  def debug[F[_]: Sync](msg: String, t: Throwable): F[Unit] = log[F](logger, Level.DEBUG, msg, t)
  def info[F[_]: Sync](msg: String): F[Unit]                = log[F](logger, Level.INFO, msg, null)
  def info[F[_]: Sync](msg: String, t: Throwable): F[Unit]  = log[F](logger, Level.INFO, msg, t)
  def warn[F[_]: Sync](msg: String): F[Unit]                = log[F](logger, Level.WARN, msg, null)
  def warn[F[_]: Sync](msg: String, t: Throwable): F[Unit]  = log[F](logger, Level.WARN, msg, t)
  def error[F[_]: Sync](msg: String): F[Unit]               = log[F](logger, Level.ERROR, msg, null)
  def error[F[_]: Sync](msg: String, t: Throwable): F[Unit] = log[F](logger, Level.ERROR, msg, t)
}

object Logging {
  private def log[F[_]: Sync](logger: Logger, level: Level, msg: String, t: Throwable): F[Unit] =
    Sync[F].blocking(logger.log(level, msg, t))
}
