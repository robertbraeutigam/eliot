package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, registerFactIfClear}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, FactOf}

import scala.reflect.ClassTag

/** Generates at most one instance of a fact of a given type for one key.
  */
abstract class SingleFactProcessor[KeyType <: CompilerFactKey[?]](using ClassTag[KeyType])
    extends SingleKeyTypeProcessor[KeyType] {
  override protected def generateFact(key: KeyType): CompilerIO[Unit] =
    generateSingleFact(key).flatMap(fact => registerFactIfClear(fact.asInstanceOf[CompilerFact]))

  protected def generateSingleFact(k: KeyType): CompilerIO[FactOf[KeyType]]
}
