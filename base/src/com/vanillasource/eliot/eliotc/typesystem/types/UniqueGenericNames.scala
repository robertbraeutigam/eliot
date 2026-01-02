package com.vanillasource.eliot.eliotc.typesystem.types

import cats.Monad
import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}
import com.vanillasource.eliot.eliotc.typesystem.processor.ShortUniqueIdentifiers.generateNextUniqueIdentifier
import com.vanillasource.eliot.eliotc.typesystem.processor.ShortUniqueIdentifiers

case class UniqueGenericNames(
    shortUniqueIdentifiers: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    boundNames: CachedTypeReferences = CachedTypeReferences(),
    cache: CachedTypeReferences = CachedTypeReferences()
)

object UniqueGenericNames {
  def boundType[F[_]](arg: ArgumentDefinition)(using Monad[F]): StateT[F, UniqueGenericNames, Unit] =
    CachedTypeReferences
      .added(arg.name.value, arg.typeReference)
      .transformS[UniqueGenericNames](_.boundNames, (ugn, cache) => ugn.copy(boundNames = cache))

  def getBoundType[F[_]](name: String)(using Monad[F]): StateT[F, UniqueGenericNames, Option[TypeReference]] =
    CachedTypeReferences
      .get(name)
      .transformS[UniqueGenericNames](_.boundNames, (ugn, cache) => ugn.copy(boundNames = cache))

  def generateUniqueGeneric[F[_]](source: Sourced[?], parameters: Seq[TypeReference] = Seq.empty)(using
      Monad[F]
  ): StateT[F, UniqueGenericNames, TypeReference] =
    generateNextUniqueIdentifier()
      .transformS[UniqueGenericNames](_.shortUniqueIdentifiers, (ugn, sui) => ugn.copy(shortUniqueIdentifiers = sui))
      .map(id => GenericTypeReference(source.as(id), parameters))

  def makeUnique[F[_]](typeReference: TypeReference)(using Monad[F]): StateT[F, UniqueGenericNames, TypeReference] =
    clearCache() >> makeUniqueCached(typeReference)

  private def clearCache[F[_]]()(using Monad[F]): StateT[F, UniqueGenericNames, Unit] =
    CachedTypeReferences
      .clear()
      .transformS[UniqueGenericNames](_.cache, (ugn, cache) => ugn.copy(cache = cache))

  private def makeUniqueCached[F[_]](typeReference: TypeReference)(using
      Monad[F]
  ): StateT[F, UniqueGenericNames, TypeReference] =
    typeReference match
      case DirectTypeReference(dataType, genericParameters) =>
        genericParameters.traverse(makeUniqueCached).map(DirectTypeReference(dataType, _))
      case GenericTypeReference(name, genericParameters)    =>
        CachedTypeReferences
          .get(name.value)
          .transformS[UniqueGenericNames](_.cache, (ugn, cache) => ugn.copy(cache = cache))
          .flatMap {
            case Some(cachedTypeReference) =>
              cachedTypeReference.pure[[T] =>> StateT[F, UniqueGenericNames, T]]
            case None                      =>
              for {
                uniqueParameters    <- genericParameters.traverse(makeUniqueCached)
                uniqueTypeReference <- generateUniqueGeneric(name, uniqueParameters)
                _                   <- CachedTypeReferences
                                         .added(name.value, uniqueTypeReference)
                                         .transformS[UniqueGenericNames](_.cache, (ugn, cache) => ugn.copy(cache = cache))
              } yield uniqueTypeReference
          }
}
