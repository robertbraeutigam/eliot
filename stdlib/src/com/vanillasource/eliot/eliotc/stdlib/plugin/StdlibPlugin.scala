package com.vanillasource.eliot.eliotc.stdlib.plugin

import cats.data.StateT
import cats.effect.IO
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.stdlib.eval.StdlibValueEvaluator

class StdlibPlugin extends CompilerPlugin {
  override def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit] =
    StateT
      .modify(superProcessor =>
        SequentialCompilerProcessors(
          Seq(
            superProcessor,
            StdlibValueEvaluator()
          )
        )
      )
}
