package com.vanillasource.eliot.eliotc.typesystem

import cats.{Applicative, Monad}
import cats.data.StateT
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference

import scala.annotation.tailrec

case class UniqueGenericNames(nextNameIndex: Int = 0, boundNames: Map[String, TypeReference] = Map.empty) {
  def generateCurrentName(): String = generateName(nextNameIndex, "")

  def advanceNameIndex(): UniqueGenericNames = UniqueGenericNames(nextNameIndex + 1, boundNames)

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
  def generateNextUniqueName[F[_]]()(using Monad[F]): StateT[F, UniqueGenericNames, String] =
    for {
      currentNames <- StateT.get[F, UniqueGenericNames]
      currentName   = currentNames.generateCurrentName()
      _            <- StateT.set(currentNames.advanceNameIndex())
    } yield currentName
}
