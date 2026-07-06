package com.vanillasource.eliot.eliotc.lsp.server

import java.nio.file.Path

/** Resolves the bundled compiler/runtime layer source roots the resident LSP compile must hand to `PathScanner`.
  *
  * Since CP1.5 the abstract base and the platform layers are supplied as ordinary filesystem roots — the classpath scan
  * that used to discover them is gone (CP1.5). A packaged distribution stages those layer
  * sources as plain directories beside the server jars (`ide/lsp/package.sh`) and the launcher points the
  * [[layersDirectoryProperty]] system property at the staging directory; this object turns that directory into the two
  * per-phase root lists.
  *
  * The layers directory holds one subdirectory per module — `lang`, `stdlib`, `jvm`, `compiler` — each itself a
  * `PathScanner` root (i.e. each contains `eliot/…`). The **compiler path** is the `compiler` override overlay *only*;
  * the **runtime path** is the abstract base (`lang` + `stdlib`) plus the `jvm` target. The compiler scan unions the
  * runtime path, so base + target resolve for compile-time too and the overlay's definitions supersede the platform's.
  * The user's own workspace roots are *not* included here: `LangPlugin` appends them (`LangPlugin.pathKey`) to the
  * runtime path.
  */
object BundledLayers {

  /** System property naming the directory that holds the bundled `lang`/`stdlib`/`jvm`/`compiler` layer source roots.
    * The package.sh launcher and the IntelliJ connection provider both set it to the `eliot-src` directory they stage.
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

  /** `(compilerPath, runtimePath)` from a layers directory holding `lang`/`stdlib`/`jvm`/`compiler` subdirectories. */
  def fromDirectory(layersDirectory: Path): (Seq[Path], Seq[Path]) =
    fromRoots(
      layersDirectory.resolve("lang"),
      layersDirectory.resolve("stdlib"),
      layersDirectory.resolve("jvm"),
      layersDirectory.resolve("compiler")
    )

  /** `(compilerPath, runtimePath)` from the four explicit module roots: compiler path = the `compiler` override overlay
    * only; runtime path = base (`lang` + `stdlib`) + the `jvm` target (the compiler scan unions it in).
    */
  def fromRoots(langRoot: Path, stdlibRoot: Path, jvmRoot: Path, compilerRoot: Path): (Seq[Path], Seq[Path]) =
    (Seq(compilerRoot), Seq(langRoot, stdlibRoot, jvmRoot))
}
