package com.vanillasource.eliot.eliotc.lsp.index

import com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN

/** Maps a fully-qualified name to its documentation *tile* — the rendered definition signature plus the documentation —
  * for hover. Rebuilt after every compile from the [[com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc]] facts the
  * whole-workspace driver demanded, so hovering a name shows the very tile the apidoc site renders (same signature, same
  * margin-stripped Markdown).
  *
  * Only names with something to show (a signature or a doc) are kept; a hover of anything else finds nothing here and
  * falls back to the concrete-type / declared-signature hint.
  */
final class DocIndex private (tilesByFqn: Map[ValueFQN, DocIndex.Tile]) {

  /** The documentation tile for `fqn`, or `None` if there is nothing to show for it. */
  def tileFor(fqn: ValueFQN): Option[DocIndex.Tile] = tilesByFqn.get(fqn)
}

object DocIndex {

  /** What hover renders for a name: the apidoc definition signature (e.g. `def printLine(s: String): IO[Unit]`) and the
    * documentation Markdown, either of which may be absent.
    */
  final case class Tile(signature: Option[String], doc: Option[String])

  val empty: DocIndex = new DocIndex(Map.empty)

  /** Build the index from the workspace's [[ValueDoc]] facts, keeping only names that have a signature or a doc. */
  def build(valueDocs: Seq[ValueDoc]): DocIndex =
    new DocIndex(
      valueDocs.flatMap { valueDoc =>
        val doc = valueDoc.doc.filter(_.nonEmpty)
        Option.when(valueDoc.signature.isDefined || doc.isDefined)(valueDoc.vfqn -> Tile(valueDoc.signature, doc))
      }.toMap
    )
}
