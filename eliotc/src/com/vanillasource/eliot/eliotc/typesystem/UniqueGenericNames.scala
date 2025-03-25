package com.vanillasource.eliot.eliotc.typesystem

import cats.Applicative
import cats.data.StateT

import scala.annotation.tailrec

case class UniqueGenericNames(nextNameIndex: Int = 0) {
  def reserveNextName(): (UniqueGenericNames, String) =
    (UniqueGenericNames(nextNameIndex + 1), generateName(nextNameIndex, ""))

  @tailrec
  private def generateName(remainingIndex: Int, alreadyGeneratedSuffix: String): String =
    if (remainingIndex < 0) {
      alreadyGeneratedSuffix
    } else {
      generateName(
        (remainingIndex / 26) - 1,
        ('A'.toInt + (remainingIndex % 26)).toChar.toString + alreadyGeneratedSuffix
      )
    }
}

object UniqueGenericNames {
  def reserveNextName[F[_]]()(using Applicative[F]): StateT[F, UniqueGenericNames, String] =
    StateT.apply[F, UniqueGenericNames, String](s => Applicative[F].pure(s.reserveNextName()))
}
