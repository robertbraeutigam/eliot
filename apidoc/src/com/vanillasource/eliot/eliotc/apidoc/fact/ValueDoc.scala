package com.vanillasource.eliot.eliotc.apidoc.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The documentation *tile* for one fully-qualified name — its rendered definition signature and its documentation —
  * produced by [[com.vanillasource.eliot.eliotc.apidoc.processor.ValueDocProcessor]] and shared by every consumer of
  * the doc pipeline: the apidoc HTML backend and the language server's hover, which "just shows the tile".
  *
  * [[signature]] is the exact one-line definition the apidoc page shows (`def printLine(s: String): IO[Unit]`, `type
  * IO[A]`, `ability Show[A]`), rendered through the same
  * [[com.vanillasource.eliot.eliotc.apidoc.render.SignatureRenderer]] the site uses. [[doc]] is the merged,
  * margin-stripped Markdown — single-sourced across platform layers exactly as the site is (the `/** ... */` comment on
  * the name's lowest/introducing layer wins, each ignored higher-layer duplicate reported in [[warnings]]). Either is
  * `None` when the name has no declaration / no documentation on any layer.
  */
case class ValueDoc(vfqn: ValueFQN, signature: Option[String], doc: Option[String], warnings: Seq[String])
    extends CompilerFact {
  override def key(): CompilerFactKey[ValueDoc] = ValueDoc.Key(vfqn)
}

object ValueDoc {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[ValueDoc]
}
