package com.vanillasource.eliot.eliotc.typesystem.processor

import cats.Monad
import cats.data.StateT

import scala.annotation.tailrec

case class ShortUniqueIdentifiers(nextIdentifierIndex: Int = 0) {
  def generateCurrentIdentifier(): String = generateIdentifier(nextIdentifierIndex, "$")

  def advanceIdentifierIndex(): ShortUniqueIdentifiers = ShortUniqueIdentifiers(nextIdentifierIndex + 1)

  @tailrec
  private def generateIdentifier(remainingIndex: Int, alreadyGeneratedSuffix: String): String =
    if (remainingIndex < 0) {
      alreadyGeneratedSuffix
    } else {
      generateIdentifier(
        (remainingIndex / 26) - 1,
        ('A'.toInt + (remainingIndex % 26)).toChar.toString + alreadyGeneratedSuffix
      )
    }
}

object ShortUniqueIdentifiers {
  def generateNextUniqueIdentifier[F[_]]()(using Monad[F]): StateT[F, ShortUniqueIdentifiers, String] =
    for {
      identifiers      <- StateT.get[F, ShortUniqueIdentifiers]
      currentIdentifier = identifiers.generateCurrentIdentifier()
      _                <- StateT.set(identifiers.advanceIdentifierIndex())
    } yield currentIdentifier
}
