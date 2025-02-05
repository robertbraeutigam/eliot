package com.vanillasource.eliot.eliotc.typesystem

import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

trait TypeInferenceScope {
  def inferTypeFor(typeReference: TypeReference): CompilationIO[TypeInference]

  def newScope(): CompilationIO[TypeInferenceScope]
}
