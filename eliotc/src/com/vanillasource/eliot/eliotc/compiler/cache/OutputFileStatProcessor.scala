package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Leaf processor for [[OutputFileStat]]: it just checks whether the output file is present. Because it has no fact
  * dependencies the incremental engine always recomputes it, so a deleted output is detected on the next build.
  */
class OutputFileStatProcessor extends SingleFactProcessor[OutputFileStat.Key] with Logging {
  override protected def generateSingleFact(key: OutputFileStat.Key): CompilerIO[OutputFileStat] =
    IO.blocking(OutputFileStat(key.file, key.file.exists())).to[CompilerIO]
}
