package com.vanillasource.eliot.eliotc.jvm.jargen

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}

import java.net.URI
import java.nio.file.Path

/** Serves the source of the synthesized entry-point module (`main.els`): a `main` that runs the configured user
  * `main`'s `IO` value. The content is a pure function of the configured main FQN, so the fact is an ordinary
  * generated leaf under incremental compilation — regenerated each run and validated by the equality cutoff, never
  * accepted blindly.
  *
  * This processor owns the [[SyntheticMainSourceProcessor.syntheticScheme]] namespace of
  * [[com.vanillasource.eliot.eliotc.source.content.SourceContent]]; its mount counterpart
  * ([[SyntheticMainMount]]) makes the module reachable in the runtime scan pool under the same URI.
  */
class SyntheticMainSourceProcessor(mainVfqn: ValueFQN) extends SingleKeyTypeProcessor[SourceContent.Key] {
  import SyntheticMainSourceProcessor.*

  override protected def generateFact(key: SourceContent.Key): CompilerIO[Unit] =
    if (key.uri != sourceUri) ().pure[CompilerIO]
    else registerFactIfClear(SourceContent(sourceUri, Sourced(sourceUri, PositionRange.zero, mainSource(mainVfqn))))
}

object SyntheticMainSourceProcessor {

  /** The URI scheme namespace this processor owns. */
  val syntheticScheme: String = "eliot-synthetic"

  /** The module-relative path of the synthesized entry point, at the source root: module `main`. */
  val sourcePath: Path = Path.of("main.els")

  /** The one URI in the synthetic namespace. */
  val sourceUri: URI = URI.create(s"$syntheticScheme:main.els")

  /** The synthesized entry-point value: `main::main`, what the jar's `Main-Class` bootstraps. */
  val syntheticMainVfqn: ValueFQN = ValueFQN(ModuleName(Seq(), "main"), QualifiedName("main", Qualifier.Default))

  private def mainSource(mainVfqn: ValueFQN): String =
    s"""
       |def main: Unit = apply(block(${mainVfqn.moduleName.show}::${mainVfqn.name.name}), unit)
       |""".stripMargin
}
