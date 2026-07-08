package com.vanillasource.eliot.eliotc.lsp.server

import java.io.{IOException, UncheckedIOException}
import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Discovers the Eliot source roots beneath the editor's workspace folders.
  *
  * The server bundles no layers: the base, the standard library, the platform layers and the user's own code all reach
  * the compiler on the path. But an editor hands the language server the *project folder*, not the individual source
  * roots, and neither LSP4IJ nor a Mill/BSP import marks Eliot's nested `eliot/` roots. So we recover the roots from each
  * workspace folder by convention:
  *
  *   - a directory named `eliot` with `.els` files beneath it is a *library/layer* root — module `eliot.lang.X` lives at
  *     `<root>/eliot/lang/X.els`; its `eliot-compiler/` sibling (compile-time overlay) is added by `LangPlugin`;
  *   - any other directory that *directly* contains `.els` files is an *application* root — module `X` at `<root>/X.els`.
  *
  * A discovered root is not descended into (so the `eliot` package dir *inside* a layer root is not mistaken for a second
  * root), and build/hidden/`eliot-compiler` directories are skipped. The workspace folder itself is never taken as a
  * layer root even when it is (like the compiler repo) a directory literally named `eliot` — it is the project, so it is
  * descended into unless it *directly* holds `.els`.
  *
  * This is a deliberately simple stopgap until a build system resolves dependencies onto the path explicitly.
  */
object SourceRootDiscovery {
  private val layerRootName         = "eliot"
  private val overlayDirectoryName  = "eliot-compiler"
  private val ignoredDirectoryNames = Set("out", "target", "build", "dist", "node_modules")
  private val eliotExtension        = ".els"
  private val maxLayerDepth         = 16

  /** The source roots beneath the given workspace folders, or the folders themselves when discovery finds none (so an
    * unconventional layout is at least attempted rather than silently unresolved).
    */
  def discover(workspaceRoots: Seq[Path]): Seq[Path] = {
    val discovered = workspaceRoots.filter(Files.isDirectory(_)).flatMap(rootsUnder).distinct
    if (discovered.isEmpty) workspaceRoots else discovered
  }

  private def rootsUnder(workspaceRoot: Path): Seq[Path] = {
    val roots = ListBuffer.empty[Path]
    def visit(directory: Path): Unit =
      if (isIgnored(directory)) ()
      else if (directory.getFileName.toString == layerRootName) {
        if (hasEliotSourceBeneath(directory)) roots += directory // layer root; an `eliot`-named dir with no Eliot
        // sources (e.g. a Scala package) is a dead branch — neither a root nor descended into.
      } else if (directlyContainsEliotSource(directory)) roots += directory // application root
      else childDirectories(directory).foreach(visit)
    // The workspace folder is the project, not a layer root, so never apply the `eliot`-name rule to it.
    if (directlyContainsEliotSource(workspaceRoot)) roots += workspaceRoot
    else childDirectories(workspaceRoot).foreach(visit)
    roots.toList
  }

  private def isIgnored(directory: Path): Boolean = {
    val name = directory.getFileName.toString
    name.startsWith(".") || name == overlayDirectoryName || ignoredDirectoryNames.contains(name)
  }

  private def directlyContainsEliotSource(directory: Path): Boolean = listing(directory).exists(isEliotSource)

  private def childDirectories(directory: Path): Seq[Path] = listing(directory).filter(Files.isDirectory(_))

  private def hasEliotSourceBeneath(directory: Path): Boolean =
    safely(false)(Using.resource(Files.walk(directory, maxLayerDepth))(_.iterator().asScala.exists(isEliotSource)))

  private def listing(directory: Path): Seq[Path] =
    safely(Seq.empty[Path])(Using.resource(Files.list(directory))(_.iterator().asScala.toList))

  private def isEliotSource(path: Path): Boolean =
    Files.isRegularFile(path) && path.getFileName.toString.endsWith(eliotExtension)

  private def safely[A](fallback: A)(body: => A): A =
    try body
    catch { case _: IOException | _: UncheckedIOException => fallback }
}
