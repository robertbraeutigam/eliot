package com.vanillasource.eliot.eliotc.lsp

import com.vanillasource.eliot.eliotc.lsp.server.BundledLayers
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}

import java.nio.file.Path

/** Shared helper for the LSP *compile* tests. Since CP1.5 removed the classpath scan, a real compile needs the abstract
  * base and the `jvm` layer handed in as filesystem roots. A forked test JVM's working dir is a per-worker sandbox, so
  * the build passes the repo root in via the `ELIOT_REPO_ROOT` env var (see `build.mill`); each module's `eliot/`
  * source root is resolved under it.
  */
object LspCompileTestLayers {
  private val repoRoot                         =
    Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
  private def moduleRoot(module: String): Path = repoRoot.resolve(module).resolve("eliot")

  private val (compilerPaths, runtimePaths): (Seq[Path], Seq[Path]) =
    BundledLayers.fromRoots(moduleRoot("lang"), moduleRoot("stdlib"), moduleRoot("jvm"), moduleRoot("compiler"))

  /** Add the compiler/runtime layer paths (compiler = base + the `compiler` platform layer; runtime = base + `jvm`
    * `eliot/` source roots) to a test configuration.
    */
  def add(configuration: Configuration): Configuration =
    configuration
      .set(LangPlugin.compilerPathKey, compilerPaths)
      .set(LangPlugin.runtimePathKey, runtimePaths)
}
