package com.vanillasource.eliot.eliotc.typesystem

import cats.syntax.all.*
import cats.Monad
import cats.data.StateT
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}

import scala.annotation.tailrec

// TODO: This is 3 things in one, split this up
case class UniqueGenericNames(
    nextNameIndex: Int = 0,
    boundNames: Map[String, TypeReference] = Map.empty,
    cache: Map[String, TypeReference] = Map.empty
) {
  def generateCurrentName(): String = generateName(nextNameIndex, "")

  def advanceNameIndex(): UniqueGenericNames = UniqueGenericNames(nextNameIndex + 1, boundNames, cache)

  def boundType(name: String, typeReference: TypeReference): UniqueGenericNames =
    UniqueGenericNames(nextNameIndex, boundNames + (name -> typeReference), cache)

  def addToCache(name: String, typeReference: TypeReference): UniqueGenericNames =
    UniqueGenericNames(nextNameIndex, boundNames, cache + (name -> typeReference))

  def clearCache(): UniqueGenericNames =
    UniqueGenericNames(nextNameIndex, boundNames)

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

  def makeUnique[F[_]](typeReference: TypeReference)(using Monad[F]): StateT[F, UniqueGenericNames, TypeReference] =
    clearCache() >> makeUniqueCached(typeReference)

  private def clearCache[F[_]]()(using Monad[F]): StateT[F, UniqueGenericNames, Unit] =
    StateT.modifyF[F, UniqueGenericNames](s => Monad[F].pure(s.clearCache()))

  private def makeUniqueCached[F[_]](typeReference: TypeReference)(using
      Monad[F]
  ): StateT[F, UniqueGenericNames, TypeReference] =
    typeReference match
      case DirectTypeReference(dataType, genericParameters) =>
        genericParameters.traverse(makeUniqueCached).map(DirectTypeReference(dataType, _))
      case GenericTypeReference(name, genericParameters)    =>
        for {
          currentNames        <- StateT.get[F, UniqueGenericNames]
          uniqueTypeReference <- currentNames.cache.get(name.value) match
                                   case Some(cachedTypeReference) =>
                                     cachedTypeReference.pure[[T] =>> StateT[F, UniqueGenericNames, T]]
                                   case None                      =>
                                     for {
                                       uniqueParameters   <- genericParameters.traverse(makeUniqueCached)
                                       currentNames       <- StateT.get[F, UniqueGenericNames]
                                       uniqueName          = currentNames.generateCurrentName()
                                       uniqueTypeReference = GenericTypeReference(name.as(uniqueName), uniqueParameters)
                                       _                  <-
                                         StateT.set(
                                           currentNames.advanceNameIndex().addToCache(name.value, uniqueTypeReference)
                                         )
                                     } yield uniqueTypeReference
        } yield uniqueTypeReference

  def boundType[F[_]](arg: ArgumentDefinition)(using Monad[F]): StateT[F, UniqueGenericNames, Unit] =
    for {
      uniqueTypeReference <- makeUnique(arg.typeReference)
      _                   <- StateT.modifyF[F, UniqueGenericNames](names =>
                               Monad[F].pure(names.boundType(arg.name.value, uniqueTypeReference))
                             )
    } yield ()

  def generateNextUniqueName[F[_]]()(using Monad[F]): StateT[F, UniqueGenericNames, String] =
    for {
      currentNames <- StateT.get[F, UniqueGenericNames]
      currentName   = currentNames.generateCurrentName()
      _            <- StateT.set(currentNames.advanceNameIndex())
    } yield currentName
}
