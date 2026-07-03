package com.vanillasource.eliot.eliotc.lsp.index

import com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN

/** Maps a fully-qualified name to its documentation, for hover. Rebuilt after every compile from the
  * [[com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc]] facts the whole-workspace driver demanded — the exact same
  * merged, margin-stripped Markdown the apidoc site renders, so hovering a name shows what its API docs would.
  *
  * Only names that actually carry documentation are kept; a hover of an undocumented name simply finds nothing here and
  * falls back to just the signature/type.
  */
final class DocIndex private (docsByFqn: Map[ValueFQN, String]) {

  /** The documentation Markdown for `fqn`, or `None` if it is undocumented (or not in the workspace). */
  def docFor(fqn: ValueFQN): Option[String] = docsByFqn.get(fqn)
}

object DocIndex {
  val empty: DocIndex = new DocIndex(Map.empty)

  /** Build the index from the workspace's [[ValueDoc]] facts, keeping only the names with a non-empty doc. */
  def build(valueDocs: Seq[ValueDoc]): DocIndex =
    new DocIndex(valueDocs.flatMap(valueDoc => valueDoc.doc.filter(_.nonEmpty).map(valueDoc.vfqn -> _)).toMap)
}
