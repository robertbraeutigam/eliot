package com.vanillasource.eliot.eliotc.stdlib.plugin

import cats.data.StateT
import cats.effect.IO
import com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors

class StdlibPlugin extends CompilerPlugin {

  /** Register this layer's native contributor label in the binding-merger roster. All plugins' `configure()` complete
    * before any `initialize()`, so the merger (built in `LangPlugin.initialize`) already sees `stdlib` and consults
    * [[StdlibNativesProcessor]] for the compile-time arithmetic natives.
    */
  override def configure(): StateT[IO, Configuration, Unit] =
    StateT.modify(
      _.updatedWith(ContributedBinding.extraNativeLabelsKey, existing =>
        Some(existing.getOrElse(Set.empty) + StdlibNativesProcessor.stdlibLabel)
      )
    )

  override def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit] =
    StateT
      .modify(superProcessor =>
        SequentialCompilerProcessors(
          Seq(
            StdlibNativesProcessor(),
            superProcessor
          )
        )
      )
}
