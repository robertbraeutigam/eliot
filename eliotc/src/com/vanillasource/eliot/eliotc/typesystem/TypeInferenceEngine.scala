package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.{IO, Ref}
import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

class TypeInferenceEngine(nextScopeId: Ref[CompilationIO, Int], thisScopeId: Int) extends TypeInferenceScope {
  override def inferTypeFor(typeReference: TypeReference): CompilationIO[TypeInference] = ???

  override def newScope(): CompilationIO[TypeInferenceScope] = for {
    newScopeId <- nextScopeId.getAndUpdate(_ + 1)
  } yield new TypeInferenceEngine(nextScopeId, newScopeId)
}

object TypeInferenceEngine {
  def apply(): CompilationIO[TypeInferenceEngine] = for {
    scopeId <- Ref.of[CompilationIO, Int](1)
  } yield new TypeInferenceEngine(scopeId, 0)
}
