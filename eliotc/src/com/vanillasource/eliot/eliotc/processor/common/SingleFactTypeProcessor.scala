package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import scala.reflect.ClassTag

/** Produces only one type of fact. When asked to generate a fact key that matches the expected type K, it delegates to
  * the abstract generateFact method. Otherwise, it does nothing.
  *
  * @tparam F
  *   The type of fact this processor generates
  * @tparam K
  *   The type of key for the fact
  */
abstract class SingleFactTypeProcessor[F <: CompilerFact, K <: CompilerFactKey[F]](using ct: ClassTag[K])
    extends SingleKeyTypeProcessor[K] {

  override protected def processKey(key: K): CompilerIO[Unit] = generateFact(key)

  /** Generate the fact for the given key. Subclasses must implement this method to provide the actual fact generation
    * logic.
    *
    * @param key
    *   The correctly-typed key for the fact to generate
    * @return
    *   A CompilerIO that registers the generated fact
    */
  protected def generateFact(key: K): CompilerIO[Unit]
}
