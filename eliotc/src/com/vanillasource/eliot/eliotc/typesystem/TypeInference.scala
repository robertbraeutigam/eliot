package com.vanillasource.eliot.eliotc.typesystem

import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

trait TypeInference extends TypeInferenceScope {
  def receivesFrom(typeReference: TypeReference): CompilationIO[TypeInference]
}
