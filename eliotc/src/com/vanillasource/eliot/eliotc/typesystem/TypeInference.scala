package com.vanillasource.eliot.eliotc.typesystem

import cats.syntax.all.*
import cats.effect.Ref
import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

class TypeInference private (
    mainTypeReference: TypeReference,
    scope: Ref[CompilationIO, Map[String, TypeInference]],
    equalTo: Ref[CompilationIO, Seq[TypeInference]]
) {
  def receivesFrom(typeReference: TypeReference): CompilationIO[TypeInference] = ???

  def inferTypeFor(typeReference: TypeReference): CompilationIO[TypeInference] = typeReference match
    case TypeReference.DirectTypeReference(_)     => newSameScopeInference(typeReference)
    case TypeReference.GenericTypeReference(name) =>
      for {
        scopeMap     <- scope.get
        newInference <-
          scopeMap.get(name.value).map(_.pure[CompilationIO]).getOrElse(newSameScopeInference(typeReference))
      } yield newInference

  private def newSameScopeInference(typeReference: TypeReference) = for {
    emptyEqualTo <- Ref.of[CompilationIO, Seq[TypeInference]](Seq.empty)
  } yield TypeInference(typeReference, scope, emptyEqualTo)

}

object TypeInference {
  def forType(typeReference: TypeReference): CompilationIO[TypeInference] = for {
    emptyScope   <- Ref.of[CompilationIO, Map[String, TypeInference]](Map.empty)
    emptyEqualTo <- Ref.of[CompilationIO, Seq[TypeInference]](Seq.empty)
  } yield TypeInference(typeReference, emptyScope, emptyEqualTo)
}
