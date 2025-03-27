package com.vanillasource.eliot.eliotc.typesystem

import cats.syntax.all.*
import cats.Monad
import cats.data.StateT
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.ShortUniqueIdentifiers.generateNextUniqueIdentifier

import scala.annotation.tailrec

// TODO: This is 3 things in one, split this up
case class UniqueGenericNames(
    shortUniqueIdentifiers: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    boundNames: Map[String, TypeReference] = Map.empty,
    cache: Map[String, TypeReference] = Map.empty
) {
  def boundType(name: String, typeReference: TypeReference): UniqueGenericNames =
    UniqueGenericNames(shortUniqueIdentifiers, boundNames + (name -> typeReference), cache)

  def addToCache(name: String, typeReference: TypeReference): UniqueGenericNames =
    UniqueGenericNames(shortUniqueIdentifiers, boundNames, cache + (name -> typeReference))

  def clearCache(): UniqueGenericNames =
    UniqueGenericNames(shortUniqueIdentifiers, boundNames)
}

object UniqueGenericNames {
  def getBoundType[F[_]](name: String)(using Monad[F]): StateT[F, UniqueGenericNames, TypeReference] =
    for {
      currentNames <- StateT.get[F, UniqueGenericNames]
    } yield currentNames.boundNames.apply(name)

  def generateUniqueGeneric[F[_]](source: Sourced[_], parameters: Seq[TypeReference] = Seq.empty)(using
      Monad[F]
  ): StateT[F, UniqueGenericNames, TypeReference] =
    generateNextUniqueIdentifier()
      .transformS[UniqueGenericNames](_.shortUniqueIdentifiers, (ugn, sui) => ugn.copy(shortUniqueIdentifiers = sui))
      .map(id => GenericTypeReference(source.as(id), parameters))

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
                                       uniqueParameters    <- genericParameters.traverse(makeUniqueCached)
                                       uniqueTypeReference <- generateUniqueGeneric(name, uniqueParameters)
                                       _                   <- StateT.modify[F, UniqueGenericNames](
                                                                _.addToCache(name.value, uniqueTypeReference)
                                                              )
                                     } yield uniqueTypeReference
        } yield uniqueTypeReference

  def boundType[F[_]](arg: ArgumentDefinition)(using Monad[F]): StateT[F, UniqueGenericNames, Unit] =
    StateT.modifyF[F, UniqueGenericNames](names => Monad[F].pure(names.boundType(arg.name.value, arg.typeReference)))
}
