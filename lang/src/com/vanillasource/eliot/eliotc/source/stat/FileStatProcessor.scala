package com.vanillasource.eliot.eliotc.source.stat

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

import java.time.Instant

/** Generates file stats for a given file. This processor never fails.
  */
class FileStatProcessor extends SingleFactProcessor[FileStat.Key] with Logging {
  override protected def generateSingleFact(key: FileStat.Key): CompilerIO[FileStat] =
    IO(key.file.lastModified()).attempt
      .map(_.toOption)
      .map(lastModified => FileStat(key.file, lastModified.filter(_ > 0L).map(Instant.ofEpochMilli)))
      .to[CompilerIO]
}
