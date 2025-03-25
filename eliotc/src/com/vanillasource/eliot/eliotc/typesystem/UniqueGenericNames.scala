package com.vanillasource.eliot.eliotc.typesystem

import cats.Monad
import cats.data.StateT
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}

import scala.annotation.tailrec

case class UniqueGenericNames(nextNameIndex: Int = 0, boundNames: Map[String, TypeReference] = Map.empty) {
  def generateCurrentName(): String = generateName(nextNameIndex, "")

  def advanceNameIndex(): UniqueGenericNames = UniqueGenericNames(nextNameIndex + 1, boundNames)

  def boundType(name: String, typeReference: TypeReference): UniqueGenericNames =
    UniqueGenericNames(nextNameIndex, boundNames + (name -> typeReference))

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
  def getBoundType[F[_]](name: String)(using Monad[F]): StateT[F, UniqueGenericNames, TypeReference] =
    for {
      currentNames <- StateT.get[F, UniqueGenericNames]
    } yield currentNames.boundNames.apply(name)

  def boundType[F[_]](arg: ArgumentDefinition)(using Monad[F]): StateT[F, UniqueGenericNames, Unit] =
    StateT.modifyF[F, UniqueGenericNames](names => Monad[F].pure(names.boundType(arg.name.value, arg.typeReference)))

  def generateNextUniqueName[F[_]]()(using Monad[F]): StateT[F, UniqueGenericNames, String] =
    for {
      currentNames <- StateT.get[F, UniqueGenericNames]
      currentName   = currentNames.generateCurrentName()
      _            <- StateT.set(currentNames.advanceNameIndex())
    } yield currentName
}
