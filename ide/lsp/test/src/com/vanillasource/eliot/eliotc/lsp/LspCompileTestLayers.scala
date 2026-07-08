package com.vanillasource.eliot.eliotc.lsp

import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}

import java.nio.file.Path

/** Shared helper for the LSP *compile* tests. A real compile needs the abstract base and the `jvm` layer on the path,
  * exactly like the standard library is for any program — the language server never bundles them, it resolves them from
  * the roots it is given. A forked test JVM's working dir is a per-worker sandbox, so the build passes the repo root in
  * via the `ELIOT_REPO_ROOT` env var (see `build.mill`); each module's `eliot/` source root is resolved under it.
  */
object LspCompileTestLayers {
  private val repoRoot                         =
    Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
  private def moduleRoot(module: String): Path = repoRoot.resolve(module).resolve("eliot")

  private val layerPaths: Seq[Path] =
    Seq(moduleRoot("lang"), moduleRoot("stdlib"), moduleRoot("jvm"))

  /** Add the base + `jvm` layer `eliot/` source roots to a test configuration, *appending* to any workspace roots the
    * test already put in `LangPlugin.pathKey`. Each root's sibling `eliot-compiler/` overlay (only `stdlib` ships one)
    * is added to the compile-time pool by `LangPlugin`.
    */
  def add(configuration: Configuration): Configuration =
    configuration.updatedWith(LangPlugin.pathKey, existing => Some(existing.getOrElse(Seq.empty) ++ layerPaths))
}
