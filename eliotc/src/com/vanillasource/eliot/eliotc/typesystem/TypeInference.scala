package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.Ref
import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

class TypeInference private (
    mainTypeReference: TypeReference,
    scope: Ref[CompilationIO, Map[String, TypeInference]],
    equalTo: Ref[CompilationIO, Seq[TypeInference]]
) {
  def receivesFrom(typeReference: TypeReference): CompilationIO[TypeInference] = ???

  def inferTypeFor(typeReference: TypeReference): CompilationIO[TypeInference] = ???
}

object TypeInference {
  def forReturnType(typeReference: TypeReference): CompilationIO[TypeInference] = for {
    scope   <- Ref.of[CompilationIO, Map[String, TypeInference]](Map.empty)
    equalTo <- Ref.of[CompilationIO, Seq[TypeInference]](Seq.empty)
  } yield TypeInference(typeReference, scope, equalTo)
}
