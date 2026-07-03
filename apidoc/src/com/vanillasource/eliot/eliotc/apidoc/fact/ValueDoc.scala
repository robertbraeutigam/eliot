package com.vanillasource.eliot.eliotc.apidoc.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The merged, margin-stripped documentation for one fully-qualified name, produced by
  * [[com.vanillasource.eliot.eliotc.apidoc.processor.ValueDocProcessor]] and shared by every consumer of the doc
  * pipeline: the apidoc HTML backend and the language server's hover.
  *
  * The doc is single-sourced across platform layers exactly as the site is — the `/** ... */` comment on the name's
  * lowest (introducing) layer wins, and each ignored higher-layer duplicate is reported in [[warnings]]. [[doc]] is
  * `None` when the name carries no documentation on any layer.
  */
case class ValueDoc(vfqn: ValueFQN, doc: Option[String], warnings: Seq[String]) extends CompilerFact {
  override def key(): CompilerFactKey[ValueDoc] = ValueDoc.Key(vfqn)
}

object ValueDoc {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[ValueDoc]
}
