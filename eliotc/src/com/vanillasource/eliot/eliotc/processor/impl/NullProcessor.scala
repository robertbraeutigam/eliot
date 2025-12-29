package com.vanillasource.eliot.eliotc.processor.impl

import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

class NullProcessor extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] = Monad[CompilerIO].unit
}
