package com.vanillasource.eliot.eliotc.typesystem.types

import cats.Monad
import cats.data.StateT
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference

case class CachedTypeReferences(cache: Map[String, TypeReference] = Map.empty) {
  def added(name: String, typeReference: TypeReference): CachedTypeReferences =
    CachedTypeReferences(cache + (name -> typeReference))

  def get(name: String): Option[TypeReference] =
    cache.get(name)

  def clear(): CachedTypeReferences =
    CachedTypeReferences()
}

object CachedTypeReferences {
  def clear[F[_]]()(using Monad[F]): StateT[F, CachedTypeReferences, Unit] =
    StateT.modifyF[F, CachedTypeReferences](s => Monad[F].pure(s.clear()))

  def added[F[_]](name: String, typeReference: TypeReference)(using Monad[F]): StateT[F, CachedTypeReferences, Unit] =
    StateT.modifyF[F, CachedTypeReferences](s => Monad[F].pure(s.added(name, typeReference)))

  def get[F[_]](name: String)(using Monad[F]): StateT[F, CachedTypeReferences, Option[TypeReference]] =
    StateT.get[F, CachedTypeReferences].map(_.get(name))
}
