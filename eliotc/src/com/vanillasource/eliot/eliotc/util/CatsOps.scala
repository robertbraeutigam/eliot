package com.vanillasource.eliot.eliotc.util

import cats.Functor
import cats.Monad
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*

object CatsOps {

  type OptionTF[F[_]] = [X] =>> OptionT[F, X]

  type OptionTIO = OptionTF[IO]

  extension [F[_], A](value: F[Option[A]]) {
    def toOptionT: OptionT[F, A] = OptionT(value)

    def onNone(action: F[Unit])(using Monad[F]): OptionT[F, A] =
      OptionT {
        value.flatMap {
          case Some(a) => value
          case None    => action >> value
        }
      }
  }

  extension [F[_]: Monad, A](value: F[A]) {
    def liftOptionT: OptionT[F, A] = OptionT.liftF(value)

    def liftOptionTNone[B]: OptionT[F, B] = liftOptionT.flatMap(_ => OptionT.none)
  }
}
