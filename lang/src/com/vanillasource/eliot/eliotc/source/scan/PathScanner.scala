package com.vanillasource.eliot.eliotc.source.scan

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.plugin.Configuration
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

import java.nio.file.Path

/** Resolves a module path against the *platform-scoped* pool of source mounts (the "compiler as a platform" plan,
  * CP1.5).
  *
  * There are two explicit mount lists — a **compiler pool** and a **runtime pool** — and [[PathScan.Key]]'s
  * [[Platform]] marker selects which one is scanned for a given request. Each pool is the complete set of source
  * namespaces for its phase: the compiler pool is the abstract base (and, from CP2, the compiler-platform layer); the
  * runtime pool is the base, the selected target (`jvm`), the user's program, and any target-contributed mounts (e.g.
  * the jvm backend's synthesized `main.els` module).
  *
  * Mounts ([[SourceMount]]) are the namespace mechanism: most are [[FilesystemMount]]s built from the CLI root paths,
  * but a plugin can contribute additional mounts through [[PathScanner.extraRuntimeMountsKey]], or substitute the
  * filesystem mount construction through [[PathScanner.mountFactoryKey]] (the LSP routes workspace files with unsaved
  * editor buffers to its own `vfs:` namespace this way). Because a module may resolve in several mounts, the resulting
  * `PathScan` carries every hit — the ordinary layer merge then unifies (or rejects) the co-located definitions, so a
  * user file colliding with a synthesized module surfaces as a loud merge error rather than silent shadowing.
  */
class PathScanner(compilerMounts: Seq[SourceMount], runtimeMounts: Seq[SourceMount])
    extends SingleFactProcessor[PathScan.Key]
    with Logging {

  private def mountsFor(platform: Platform): Seq[SourceMount] = platform match {
    case Platform.Compiler => compilerMounts
    case Platform.Runtime  => runtimeMounts
  }

  override protected def generateSingleFact(key: PathScan.Key): CompilerIO[PathScan] =
    for {
      fileUris <- mountsFor(key.platform).toList.traverse(_.resolve(key.path)).map(_.flatten.distinct)
      _        <- debug[CompilerIO](s"Found files (${key.platform}): ${fileUris.mkString(", ")}")
      _        <- abortOnMissing(key).whenA(fileUris.isEmpty)
    } yield PathScan(key.path, fileUris, key.platform)

  /** How an empty scan (no mount has the path in this platform's pool) is reported, which depends on the marker:
    *
    *   - [[Platform.Runtime]] is the build itself — the program plus the stdlib/target it needs. A module that should be
    *     there but is absent from the explicit pool is a real misconfiguration, so it hard-errors (no silent fallback,
    *     CP1.5).
    *   - [[Platform.Compiler]] is an *overlay* of compile-time reductions over the base, queried for *every* name by the
    *     compiler-native contributor ([[com.vanillasource.eliot.eliotc.monomorphize.processor.CompilerNativesProcessor]]).
    *     Most names — all user code, every runtime-only/jvm-only name — name a module that is simply not in the compiler
    *     pool, which is normal, not an error. So a compiler-pool miss aborts (declines) **silently**: the contributor
    *     reads it back as "no compile-time override" and answers `None`. A genuinely missing compiler-platform layer
    *     surfaces instead as a use-site reduction failure, and a missing base still hard-errors on the runtime pool
    *     (base is in both).
    */
  private def abortOnMissing(key: PathScan.Key): CompilerIO[Unit] =
    key.platform match {
      case Platform.Runtime  =>
        compilerGlobalError(
          s"Could not find path ${key.path} at given ${key.platform} mounts: ${mountsFor(key.platform).mkString(", ")}"
        ).to[CompilerIO] >> abort
      case Platform.Compiler => abort
    }
}

object PathScanner {

  /** Substitutes how filesystem root paths become mounts; the LSP sets this to route files with unsaved editor buffers
    * to its `vfs:` namespace. Absent means plain [[FilesystemMount]].
    */
  val mountFactoryKey: Configuration.Key[Path => SourceMount] = namedKey[Path => SourceMount]("sourceMountFactory")

  /** Additional runtime-pool mounts contributed by plugins (e.g. the jvm target's synthesized `main.els` module). */
  val extraRuntimeMountsKey: Configuration.Key[Seq[SourceMount]] = namedKey[Seq[SourceMount]]("extraRuntimeMounts")
}
