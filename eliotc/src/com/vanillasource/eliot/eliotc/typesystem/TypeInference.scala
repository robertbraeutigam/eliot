package com.vanillasource.eliot.eliotc.typesystem

import cats.syntax.all.*
import cats.effect.Ref
import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

class TypeInference private (
    unifiedTypeReference: Ref[CompilationIO, TypeReference],
    scope: Ref[CompilationIO, Map[String, TypeInference]],
    equalTo: Ref[CompilationIO, Seq[TypeInference]]
) {
  def receivesFrom(typeReference: TypeReference): CompilationIO[TypeInference] = for {
    newTypeReference <- Ref[CompilationIO].of(typeReference)
    emptyScope       <- Ref.of[CompilationIO, Map[String, TypeInference]](Map.empty)
    newEqualTo       <- Ref.of[CompilationIO, Seq[TypeInference]](Seq(this))
    newInference      = TypeInference(newTypeReference, emptyScope, newEqualTo)
    _                <- equalTo.update(newInference +: _)
    _                <- unifyWith(typeReference)
  } yield newInference

  def inferTypeFor(typeReference: TypeReference): CompilationIO[TypeInference] = typeReference match
    case TypeReference.DirectTypeReference(_)     => newSameScopeInference(typeReference)
    case TypeReference.GenericTypeReference(name) =>
      for {
        scopeMap  <- scope.get
        inference <- scopeMap.get(name.value).map(_.pure[CompilationIO]).getOrElse(newSameScopeInference(typeReference))
        _         <- scope.set(scopeMap ++ Seq((name.value, inference)))
      } yield inference

  private def newSameScopeInference(typeReference: TypeReference) = for {
    newTypeReference <- Ref[CompilationIO].of(typeReference)
    emptyEqualTo     <- Ref.of[CompilationIO, Seq[TypeInference]](Seq.empty)
  } yield TypeInference(newTypeReference, scope, emptyEqualTo)

  private def unifyWith(incoming: TypeReference): CompilationIO[Unit] = ???
}

object TypeInference {
  def forType(typeReference: TypeReference): CompilationIO[TypeInference] = for {
    newTypeReference <- Ref[CompilationIO].of(typeReference)
    emptyScope       <- Ref.of[CompilationIO, Map[String, TypeInference]](Map.empty)
    emptyEqualTo     <- Ref.of[CompilationIO, Seq[TypeInference]](Seq.empty)
  } yield TypeInference(newTypeReference, emptyScope, emptyEqualTo)
}
