package com.vanillasource.eliot.eliotc.processor

import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO

/** All processors of the compiler must implement this trait to generate facts. When someone requests a fact from the
  * engine which is not yet present, the engine will ask processors to generate the fact through this interface.
  */
trait CompilerProcessor {

  /** Generate the fact with the given key, if able. Otherwise, do nothing.
    */
  def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit]
}
