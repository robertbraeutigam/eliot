package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, registerFactIfClear}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, FactOf}

import scala.reflect.ClassTag

/** Generates at most one instance of a fact of a given type for one key.
  */
abstract class SingleFactProcessor[K <: CompilerFactKey[?]](using
    ct: ClassTag[K],
    ev: FactOf[K] <:< CompilerFact
) extends SingleKeyTypeProcessor[K] {
  override protected def generateFact(key: K): CompilerIO[Unit] =
    generateSingleFact(key).flatMap(fact => registerFactIfClear(ev(fact)))

  protected def generateSingleFact(k: K): CompilerIO[FactOf[K]]
}
