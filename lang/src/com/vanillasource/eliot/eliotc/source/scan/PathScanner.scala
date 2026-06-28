package com.vanillasource.eliot.eliotc.source.scan

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.stat.FileStat

import java.nio.file.Path

/** Resolves a module path against the *platform-scoped* pool of source roots (the "compiler as a platform" plan, CP1.5).
  *
  * There are two explicit filesystem root lists — a **compiler path** and a **runtime path** — and [[PathScan.Key]]'s
  * [[Platform]] marker selects which one is scanned for a given request. Each list is the complete set of filesystem
  * roots for its phase: the compiler path is the abstract base (and, from CP2, the compiler-platform layer); the runtime
  * path is the base, the selected target (`jvm`), and the user's program.
  *
  * These two filesystem lists are the **sole** source of `.els` (CP1.5): the former blanket classpath scan
  * (`getResources("eliot/…")`) is gone. Every layer — the abstract base and `jvm` — is supplied as an ordinary
  * filesystem path, exactly as the user's program is; packaged tooling stages the layer sources as plain directories
  * (see `ide/lsp/package.sh`) so no `Path` ever has to resolve into a jar. Because each platform's roots are listed
  * explicitly per marker, a name concrete in the compiler platform and a name concrete in `jvm` can no longer collide in
  * a single pool.
  */
class PathScanner(compilerRootPaths: Seq[Path], runtimeRootPaths: Seq[Path])
    extends SingleFactProcessor[PathScan.Key]
    with Logging {

  private def rootsFor(platform: Platform): Seq[Path] = platform match {
    case Platform.Compiler => compilerRootPaths
    case Platform.Runtime  => runtimeRootPaths
  }

  override protected def generateSingleFact(key: PathScan.Key): CompilerIO[PathScan] = {
    val rootPaths = rootsFor(key.platform)
    for {
      contentFacts <- rootPaths
                        .map(_.resolve(key.path).toFile)
                        .toList
                        .traverse(file => getFactOrAbort(FileStat.Key(file)))
      fileUris      = contentFacts.filter(_.lastModified.isDefined).map(_.file.toURI)
      _            <- debug[CompilerIO](s"Found files (${key.platform}): ${fileUris.mkString(", ")}")
      _            <- (compilerGlobalError(
                        s"Could not find path ${key.path} at given ${key.platform} roots: ${rootPaths.mkString(", ")}"
                      ).to[CompilerIO] >> abort).whenA(fileUris.isEmpty)
    } yield PathScan(key.path, fileUris, key.platform)
  }
}
