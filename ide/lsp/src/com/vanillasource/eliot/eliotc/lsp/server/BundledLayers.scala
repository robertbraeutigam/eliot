package com.vanillasource.eliot.eliotc.lsp.server

import java.nio.file.Path

/** Resolves the bundled layer source roots the resident LSP compile must hand to `PathScanner` (via
  * `LangPlugin.pathKey`).
  *
  * The abstract base and the platform layers are supplied as ordinary filesystem roots — the classpath scan that used to
  * discover them is gone. A packaged distribution stages those layer sources as plain directories beside the server jars
  * (`ide/lsp/package.sh`) and the launcher points the [[layersDirectoryProperty]] system property at the staging
  * directory; this object turns that directory into the list of layer roots.
  *
  * The layers directory holds one subdirectory per module — `lang`, `stdlib`, `jvm` — each staging that module's
  * in-tree `eliot/` root (and, where present, its `eliot-compiler/` compile-time overlay) under `<module>/`. A layer
  * root is therefore `<layersDir>/<module>/eliot`; `LangPlugin` derives its `eliot-compiler/` sibling for the compiler
  * pool. The user's own workspace roots are *not* included here: `EliotCompilationService` appends them to the same
  * `LangPlugin.pathKey` list.
  */
object BundledLayers {

  /** System property naming the directory that holds the bundled `lang`/`stdlib`/`jvm` layer source roots. The
    * package.sh launcher and the IntelliJ connection provider both set it to the `eliot-src` directory they stage.
    */
  val layersDirectoryProperty = "eliot.layers"

  /** The layer roots from [[layersDirectoryProperty]], or an empty list when it is unset (in which case the resident
    * compile resolves only the user's own roots — every stdlib name is then unresolved, a degraded but safe state the
    * caller surfaces as a warning rather than mis-resolving).
    */
  def fromSystemProperty: Seq[Path] =
    Option(System.getProperty(layersDirectoryProperty))
      .map(directory => fromDirectory(Path.of(directory)))
      .getOrElse(Seq.empty)

  /** The layer roots from a layers directory holding `lang`/`stdlib`/`jvm` subdirectories (each with an `eliot/` tree). */
  def fromDirectory(layersDirectory: Path): Seq[Path] =
    fromRoots(
      layersDirectory.resolve("lang").resolve("eliot"),
      layersDirectory.resolve("stdlib").resolve("eliot"),
      layersDirectory.resolve("jvm").resolve("eliot")
    )

  /** The layer roots from the explicit module `eliot/` roots. Each is scanned for the runtime pool; its sibling
    * `eliot-compiler/` overlay (only `stdlib` ships one) is added for the compiler pool by `LangPlugin`.
    */
  def fromRoots(langRoot: Path, stdlibRoot: Path, jvmRoot: Path): Seq[Path] =
    Seq(langRoot, stdlibRoot, jvmRoot)
}
