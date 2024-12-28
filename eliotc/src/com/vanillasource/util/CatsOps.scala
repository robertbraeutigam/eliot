package com.vanillasource.util

import cats.Functor
import cats.Monad
import cats.data.OptionT

object CatsOps {

  extension [F[_], A](value: F[Option[A]]) {
    def toOptionT: OptionT[F, A] = OptionT(value)
  }

  extension [F[_]: Functor](opt: OptionT[F, Unit]) {
    def getOrUnit: F[Unit] = opt.getOrElse(())
  }

  extension [F[_]: Monad, A](value: F[A]) {
    def liftOptionT: OptionT[F, A] = OptionT.liftF(value)

    def liftOptionTNone[B]: OptionT[F, B] = liftOptionT.flatMap(_ => OptionT.none)
  }
}
