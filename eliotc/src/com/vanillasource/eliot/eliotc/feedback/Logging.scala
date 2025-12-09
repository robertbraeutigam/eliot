package com.vanillasource.eliot.eliotc.feedback

import cats.Functor
import cats.data.OptionT
import cats.effect.Sync
import com.vanillasource.eliot.eliotc.feedback.Logging.Log
import org.apache.logging.log4j.{Level, LogManager, Logger}

trait Logging {
  private val logger: Logger = LogManager.getLogger(this.getClass)

  def debug[F[_]: Log](msg: String): F[Unit]               = Log[F].log(logger, Level.DEBUG, msg, null)
  def debug[F[_]: Log](msg: String, t: Throwable): F[Unit] = Log[F].log(logger, Level.DEBUG, msg, t)
  def info[F[_]: Log](msg: String): F[Unit]                = Log[F].log(logger, Level.INFO, msg, null)
  def info[F[_]: Log](msg: String, t: Throwable): F[Unit]  = Log[F].log(logger, Level.INFO, msg, t)
  def warn[F[_]: Log](msg: String): F[Unit]                = Log[F].log(logger, Level.WARN, msg, null)
  def warn[F[_]: Log](msg: String, t: Throwable): F[Unit]  = Log[F].log(logger, Level.WARN, msg, t)
  def error[F[_]: Log](msg: String): F[Unit]               = Log[F].log(logger, Level.ERROR, msg, null)
  def error[F[_]: Log](msg: String, t: Throwable): F[Unit] = Log[F].log(logger, Level.ERROR, msg, t)
}

object Logging {
  trait Log[F[_]] {
    def log(logger: Logger, level: Level, msg: String, t: Throwable): F[Unit]
  }

  object Log {
    def apply[F[_]: Log]: Log[F] = implicitly
  }

  given logFromSync[F[_]: Sync]: Log[F] with
    override def log(logger: Logger, level: Level, msg: String, t: Throwable): F[Unit] =
      Sync[F].blocking(logger.log(level, msg, t))

  given logFromOptionT[F[_]: Functor: Log]: Log[[A] =>> OptionT[F, A]] with
    override def log(logger: Logger, level: Level, msg: String, t: Throwable): OptionT[F, Unit] =
      OptionT.liftF(Log[F].log(logger, level, msg, t))
}
