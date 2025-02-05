package com.vanillasource.eliot.eliotc.typesystem

import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

class TypeInferenceEngine() extends TypeInferenceScope {
  override def inferTypeFor(typeReference: TypeReference): CompilationIO[TypeInference] = ???

  override def newScope(): CompilationIO[TypeInferenceScope] = ???
}
