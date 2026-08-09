package com.vanillasource.eliot.eliotc.jvm.jargen

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}

import java.net.URI
import java.nio.file.Path

/** Serves the source of the synthesized entry-point module (`main.els`): a `main` that runs the configured user `main`
  * on the JVM carrier `IO`. This is where the platform *binds the carrier*: the idiomatic user `main` declares an
  * effect set (`def main: {Console} Unit`, desugared to an inferable carrier `F[Unit]`), and `block`'s expected type
  * `IO[A]` instantiates that carrier to `IO` by ordinary unification — the user program never names `IO`. A
  * deliberately JVM-pinned `main: IO[Unit]` (with `import eliot.jvm.IO`) fits the same wrapper unchanged. The content
  * is a pure function of the configured main FQN, so the fact is an ordinary generated leaf under incremental
  * compilation — regenerated each run and validated by the equality cutoff, never accepted blindly.
  *
  * This processor owns the [[SyntheticMainSourceProcessor.syntheticScheme]] namespace of
  * [[com.vanillasource.eliot.eliotc.source.content.SourceContent]]; its mount counterpart ([[SyntheticMainMount]])
  * makes the module reachable in the runtime scan pool under the same URI.
  */
class SyntheticMainSourceProcessor(mainVfqn: ValueFQN) extends SingleKeyTypeProcessor[SourceContent.Key] {
  import SyntheticMainSourceProcessor.*

  override protected def generateFact(key: SourceContent.Key): CompilerIO[Unit] =
    if (key.uri != sourceUri) ().pure[CompilerIO]
    else
      registerFactIfClear(
        SourceContent(sourceUri, Sourced(sourceUri, PositionRange.zero, carrierMainSource(mainVfqn)))
      )
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

  /** The platform **run boundary** the synthesized entry calls (`def runMain[A](io: IO[A]): A`, in `eliot.jvm.IO`). Its
    * `io: IO[A]` parameter is the one compiler-generated **carrier capture**: the user `main`'s inferable carrier
    * `?F[Unit]` binds to `IO` there. The jvm plugin registers this FQN as a
    * [[com.vanillasource.eliot.eliotc.row.RunBoundaryFunctions]] so the checker classifies that parameter's expected
    * slot as a carrier slot and routes it through the join solver — the effects-as-channel carrier-stack recognition
    * tag, source (ii) (docs/effects-as-channel.md §7 step 4 / finding 14). `IO` is jvm-owned, so this nominal boundary
    * (not the anonymous `block`/`apply` chain) is what gives the checker a taggable slot without lang ever naming `IO`.
    */
  val runMainVfqn: ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "jvm"), "IO"), QualifiedName("runMain", Qualifier.Default))

  /** The carrier-path wrapper: calling `runMain` (whose `io: IO[A]` binds the user `main`'s carrier to `IO`) makes the
    * run boundary **nominal**, and running the thunk returns `Unit`. Naming `runMain` (rather than inlining
    * `apply(block(main), unit)`) gives the checker the single taggable carrier-capture slot source (ii) routes through
    * the join ([[runMainVfqn]]).
    */
  private def carrierMainSource(mainVfqn: ValueFQN): String =
    s"""
       |import eliot.jvm.IO
       |def main: Unit = runMain(${mainVfqn.moduleName.show}::${mainVfqn.name.name})
       |""".stripMargin
}
