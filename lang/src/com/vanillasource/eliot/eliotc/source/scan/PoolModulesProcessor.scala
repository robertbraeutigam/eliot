package com.vanillasource.eliot.eliotc.source.scan

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Builds [[PoolModules]] by enumerating the mounts of the requested platform pool and unioning their module-relative
  * `.els` paths into module names. Mirrors [[PathScanner]]'s pooling: a [[Platform.Runtime]] request enumerates the
  * runtime mounts, a [[Platform.Compiler]] request enumerates the compiler-overlay mounts *and* the runtime mounts
  * (the compiler pool borrows the whole runtime track). The listing dependencies each mount records (a walked
  * directory's `FileStat`) become this fact's dependencies, so a file appearing or disappearing re-walks and rebuilds.
  *
  * Mount-dependent (constructed from the CLI roots), so — like [[PathScanner]] — it is contributed by `LangPlugin`
  * rather than living in the mount-free `LangProcessors` list.
  */
class PoolModulesProcessor(compilerMounts: Seq[SourceMount], runtimeMounts: Seq[SourceMount])
    extends SingleFactProcessor[PoolModules.Key] {

  override protected def generateSingleFact(key: PoolModules.Key): CompilerIO[PoolModules] = {
    val mounts = key.platform match {
      case Platform.Runtime  => runtimeMounts
      case Platform.Compiler => (compilerMounts ++ runtimeMounts).distinct
    }
    mounts.toList
      .flatTraverse(_.enumerate.map(_.toList))
      .map(paths => PoolModules(key.platform, paths.map(ModuleName.fromPath).toSet))
  }
}
