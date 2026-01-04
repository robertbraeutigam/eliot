package com.vanillasource.eliot.eliotc.processor.common

import cats.Monad
import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

/**
 * Does nothing.
 */
class NullProcessor extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] = Monad[CompilerIO].unit
}
