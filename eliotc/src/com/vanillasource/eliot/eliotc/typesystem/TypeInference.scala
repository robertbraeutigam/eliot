package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.Ref
import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

class TypeInference private (
    mainTypeReference: TypeReference,
    scope: Map[String, TypeInference],
    equalTo: Seq[TypeInference]
) {
  def receivesFrom(typeReference: TypeReference): CompilationIO[TypeInference] = ???

  def inferTypeFor(typeReference: TypeReference): CompilationIO[TypeInference] = ???
}

object TypeInference {
  def forReturnType(typeReference: TypeReference): TypeInference = TypeInference(typeReference, Map.empty, Seq.empty)
}
