package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, registerFactIfClear}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import scala.reflect.ClassTag

/** Generates at most one instance of a fact of a given type for one key.
  */
abstract class SingleFactProcessor[F <: CompilerFact, K <: CompilerFactKey[F]](using ct: ClassTag[K])
    extends SingleFactTypeProcessor[F, K] {
  override protected def generateFact(key: K): CompilerIO[Unit] =
    generateSingleFact(key).flatMap(registerFactIfClear(_))

  protected def generateSingleFact(k: K): CompilerIO[F]
}
