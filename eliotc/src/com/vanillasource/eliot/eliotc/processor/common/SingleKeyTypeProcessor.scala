package com.vanillasource.eliot.eliotc.processor.common

import cats.Monad
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

import scala.reflect.ClassTag

/** Processes only one type of key. When asked to generate a fact key that matches the expected type K, it delegates to
  * the abstract processKey method. Otherwise, it does nothing.
  *
  * @tparam K
  *   The type of key this processor handles
  */
abstract class SingleKeyTypeProcessor[K <: CompilerFactKey[?]](using ct: ClassTag[K]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    factKey match {
      case key: K => processKey(key)
      case _      => Monad[CompilerIO].unit
    }

  /** Process the given key. Subclasses must implement this method to provide the actual processing logic.
    *
    * @param key
    *   The correctly-typed key to process
    * @return
    *   A CompilerIO that performs the processing
    */
  protected def processKey(key: K): CompilerIO[Unit]
}
