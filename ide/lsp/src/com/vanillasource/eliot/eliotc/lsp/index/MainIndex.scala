package com.vanillasource.eliot.eliotc.lsp.index

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.lsp.virtual.VfsUris
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue

import java.net.URI
import java.nio.file.Paths

/** Index of runnable entry points: maps a document to its `main`, if it declares one.
  *
  * A module is runnable exactly when it declares a value named `main` in the default namespace — the same name the JVM
  * backend's `exe-jar` target builds a program around. This index is rebuilt from the workspace's [[ResolvedValue]]
  * facts after every compile, so the document service can answer `textDocument/codeLens` with a "Run main" lens above
  * each such definition. It carries the `main` name's source range (where the lens is anchored) and the declaring
  * [[ModuleName]] (passed as the backend's `-m` argument), keyed by the document URI.
  */
final class MainIndex private (mainsByUri: Map[String, MainIndex.Entry]) {

  /** The `main` declared in the given document, or `None` if it declares none. */
  def mainAt(uri: URI): Option[MainIndex.Entry] = mainsByUri.get(MainIndex.uriKey(uri))
}

object MainIndex {

  /** A runnable `main`: the source range of its name (the lens anchor) and the module that declares it. */
  case class Entry(range: PositionRange, moduleName: ModuleName)

  val empty: MainIndex = new MainIndex(Map.empty)

  private val mainName = QualifiedName("main", Qualifier.Default)

  /** Build the index from all resolved values in the workspace, keeping only those named `main`. A document declares at
    * most one `main`, so the last one wins on the (degenerate) duplicate case — which the compiler would already have
    * flagged as a redefinition.
    */
  def build(values: Seq[ResolvedValue]): MainIndex =
    new MainIndex(
      values
        .filter(_.vfqn.name == mainName)
        .map(value => uriKey(value.name.uri) -> Entry(value.name.range, value.vfqn.moduleName))
        .toMap
    )

  /** Normalise a URI to a stable key so the editor's `file:///…` URIs match the compiler's `file:/…` URIs, mirroring
    * [[PositionIndex]]. Non-file URIs fall back to their string form and simply never match a workspace document.
    */
  private def uriKey(uri: URI): String =
    try Paths.get(VfsUris.toFileUri(uri)).toString
    catch { case _: IllegalArgumentException | _: java.nio.file.FileSystemNotFoundException => uri.toString }
}
