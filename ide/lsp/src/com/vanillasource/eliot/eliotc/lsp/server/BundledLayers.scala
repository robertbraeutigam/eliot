package com.vanillasource.eliot.eliotc.lsp.server

import java.nio.file.Path

/** Resolves the bundled compiler/runtime layer source roots the resident LSP compile must hand to `PathScanner`.
  *
  * Since CP1.5 the abstract base and the platform layers are supplied as ordinary filesystem roots — the classpath scan
  * that used to discover them is gone (see `docs/compiler-as-platform.md`). A packaged distribution stages those layer
  * sources as plain directories beside the server jars (`ide/lsp/package.sh`) and the launcher points the
  * [[layersDirectoryProperty]] system property at the staging directory; this object turns that directory into the two
  * per-phase root lists.
  *
  * The layers directory holds one subdirectory per module — `lang`, `stdlib`, `jvm` — each itself a `PathScanner` root
  * (i.e. each contains `eliot/…`). The **compiler path** is the abstract base (`lang` + `stdlib`); the **runtime path**
  * adds the `jvm` target. The user's own workspace roots are *not* included here: `LangPlugin` appends them
  * (`LangPlugin.pathKey`) to the runtime path.
  */
object BundledLayers {

  /** System property naming the directory that holds the bundled `lang`/`stdlib`/`jvm` layer source roots. The
    * package.sh launcher and the IntelliJ connection provider both set it to the `eliot-src` directory they stage.
    */
  val layersDirectoryProperty = "eliot.layers"

  /** `(compilerPath, runtimePath)` from [[layersDirectoryProperty]], or two empty lists when it is unset (in which case
    * the resident compile resolves only the user's own roots — every stdlib name is then unresolved, a degraded but safe
    * state the caller surfaces as a warning rather than mis-resolving).
    */
  def fromSystemProperty: (Seq[Path], Seq[Path]) =
    Option(System.getProperty(layersDirectoryProperty))
      .map(directory => fromDirectory(Path.of(directory)))
      .getOrElse((Seq.empty, Seq.empty))

  /** `(compilerPath, runtimePath)` from a layers directory holding `lang`/`stdlib`/`jvm` subdirectories. */
  def fromDirectory(layersDirectory: Path): (Seq[Path], Seq[Path]) =
    fromRoots(layersDirectory.resolve("lang"), layersDirectory.resolve("stdlib"), layersDirectory.resolve("jvm"))

  /** `(compilerPath, runtimePath)` from the three explicit module roots: compiler = base (`lang` + `stdlib`), runtime
    * adds the `jvm` target.
    */
  def fromRoots(langRoot: Path, stdlibRoot: Path, jvmRoot: Path): (Seq[Path], Seq[Path]) =
    (Seq(langRoot, stdlibRoot), Seq(langRoot, stdlibRoot, jvmRoot))
}
