package com.vanillasource.eliot.eliotc.typesystem

import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

class TypeInferenceEngine() {
  def inferTypeFor(typeReference: TypeReference): CompilationIO[TypeInference] = ???
}
