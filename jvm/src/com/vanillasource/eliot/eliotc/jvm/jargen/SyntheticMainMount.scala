package com.vanillasource.eliot.eliotc.jvm.jargen

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.scan.SourceMount

import java.net.URI
import java.nio.file.Path

/** Mounts the synthesized entry-point module (`main.els`) into the runtime scan pool, resolved to the synthetic
  * namespace served by [[SyntheticMainSourceProcessor]]. Contributed by the jvm plugin via
  * [[com.vanillasource.eliot.eliotc.source.scan.PathScanner.extraRuntimeMountsKey]].
  *
  * If the user's own sources also contain a `main.els`, both resolutions end up in the `PathScan` and the ordinary
  * layer merge reports the collision loudly ("Has multiple implementations") — the module name `main` is effectively
  * reserved by this target.
  */
final class SyntheticMainMount extends SourceMount {
  override def resolve(path: Path): CompilerIO[Option[URI]] =
    Option.when(path == SyntheticMainSourceProcessor.sourcePath)(SyntheticMainSourceProcessor.sourceUri).pure[CompilerIO]

  override def toString: String = s"SyntheticMainMount(${SyntheticMainSourceProcessor.sourceUri})"
}
