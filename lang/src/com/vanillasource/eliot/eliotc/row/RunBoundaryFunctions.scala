package com.vanillasource.eliot.eliotc.row

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.plugin.Configuration

/** The platform's **run boundaries**: the values at which every effect's chain ends — the synthesized entry point,
  * and the LSP's own wrapper for it.
  *
  * Effects v6 (`docs/effects.md` §9.5): a binding is forwarded lexically, so an effect an enclosing definition does
  * not declare is the "performs but does not declare" error — *except* here. The synthesized entry point declares
  * nothing and runs a `main` that declares a row, so this is where each of that row's entries is bound to the
  * two-site `Default`: the platform's own `implement` for it, or an error naming the effect if it has none.
  *
  * A platform contributes its boundary because only the platform knows which value it is. `lang` must never name one
  * — it does not know `IO`, `runMain`, or that a jvm program has an entry point at all.
  *
  * Read by the row phase alone: `LangPlugin.initialize` passes the registered set through `LangProcessors` to
  * [[com.vanillasource.eliot.eliotc.row.processor.RowElaborationProcessor]], which tells
  * [[BindingWriter]] whether the value it is writing is one. Empty in a lang-only build and in most tests, where no
  * value is a boundary — and there an uncovered effect is simply the error it should be.
  */
object RunBoundaryFunctions {

  /** The set of boundary value FQNs a platform layer contributes in its `configure()` (the jvm plugin adds the
    * synthesized `main::main`). `LangPlugin.initialize` reads it — all `configure()`s complete before any
    * `initialize` — and threads it to the row phase via `LangProcessors`.
    */
  // Opaque to the cache identity: this set is a deterministic function of the active plugin set (fixed within a
  // compiler build, and covered by the compiler fingerprint) and the selected `main`, which already contributes.
  val configKey: Configuration.Key[Set[ValueFQN]] = Configuration.opaqueKey("runBoundaryFunctions")
}
