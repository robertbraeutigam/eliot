package com.vanillasource.eliot.eliotc.util

import cats.Functor
import cats.Monad
import cats.data.OptionT
import cats.syntax.all.*

object CatsOps {

  extension [F[_]: Monad, A](value: F[Option[A]]) {
    def toOptionT: OptionT[F, A] = OptionT(value)

    def onNone(action: F[Unit]): OptionT[F, A] =
      OptionT {
        value.flatMap {
          case Some(a) => value
          case None    => action >> value
        }
      }
  }

  extension [F[_]: Functor](opt: OptionT[F, Unit]) {
    def getOrUnit: F[Unit] = opt.getOrElse(())
  }

  extension [F[_]: Monad, A](value: F[A]) {
    def liftOptionT: OptionT[F, A] = OptionT.liftF(value)

    def liftOptionTNone[B]: OptionT[F, B] = liftOptionT.flatMap(_ => OptionT.none)
  }
}
