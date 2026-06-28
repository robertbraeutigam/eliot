package com.vanillasource.eliot.eliotc.source.scan

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.stat.FileStat

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** Resolves a module path against the *platform-scoped* pool of source roots (the "compiler as a platform" plan, CP1).
  *
  * There are two explicit filesystem root lists — a **compiler path** and a **runtime path** — and [[PathScan.Key]]'s
  * [[Platform]] marker selects which one is scanned for a given request. Each list is the complete set of filesystem
  * roots for its phase: the compiler path is the abstract base (and, from CP2, the compiler-platform layer); the runtime
  * path is the base, the selected target (`jvm`), and the user's program.
  *
  * The blanket classpath scan (`getResources("eliot/…")`) is *retained for both markers* as the CP1 development
  * intermediate: the base and `jvm` `.els` ship as classpath resources, and packaged tooling (the LSP dist, the exe-jar
  * generator) still relies on classpath discovery (see the plan's "Deferred — a real build system"). It is harmless in
  * CP1 because no name is yet defined concretely in *both* the compiler platform and a runtime layer; once CP2 adds the
  * compiler-platform module the explicit filesystem lists fully supersede it and it is dropped, so that a name concrete
  * in the compiler platform and in `jvm` cannot collide in a single pool.
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
      resourcePath  = key.path.toString.replace(java.io.File.separatorChar, '/')
      resourceUris <- IO(
                        getClass.getClassLoader
                          .getResources(s"eliot/$resourcePath")
                          .asScala
                          .map(_.toURI)
                          .toSeq
                      ).to[CompilerIO]
      allUris       = fileUris ++ resourceUris
      _            <- debug[CompilerIO](s"Found files (${key.platform}): ${allUris.mkString(", ")}")
      _            <- (compilerGlobalError(
                        s"Could not find path ${key.path} at given ${key.platform} roots: ${rootPaths.mkString(", ")}"
                      ).to[CompilerIO] >> abort).whenA(allUris.isEmpty)
    } yield PathScan(key.path, allUris, key.platform)
  }
}
