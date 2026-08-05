package com.vanillasource.eliot.eliotc.row

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.plugin.Configuration

/** The platform **run boundaries**: values whose parameter 0 *hosts* a computation on the platform's own concrete
  * carrier, rather than receiving it as data. The one instance today is the jvm entry point `eliot.jvm::runMain`
  * (`def runMain[A](io: IO[A]): A`), which the synthesized `main` hands the user program to; the LSP registers its own
  * wrapper for the same reason.
  *
  * This is the carrier-recognition tag **source (ii)**, the platform-contributed counterpart of the pinned-row desugar
  * tag ([[com.vanillasource.eliot.eliotc.ast.fact.EffectRow.pinnedParameterIndices]], source (i)): a *concrete* carrier
  * stack like `IO[A]` cannot be spelled as a pinned row (`IO` is a `Suspend`-riding carrier with no ability, and is
  * jvm-owned so `lang` must never name it), yet the elaborator still has to treat that slot as a capture rather than a
  * data container — which shape or name detection cannot do soundly. The platform that owns the carrier declares the
  * boundary here, by construction.
  *
  * Read by the row phase alone: `LangPlugin.initialize` passes the registered set through `LangProcessors` to
  * [[com.vanillasource.eliot.eliotc.row.processor.RowElaborationProcessor]], which puts it in the
  * [[RowChecker.Universe]] that both the elaborator (`carrierAt`, the capture rule) and the row verifier read. Empty in
  * a lang-only build and in most tests, where no value is a boundary.
  *
  * It used to be a *fact* as well (`RunBoundaryFunction`, produced per FQN for the v2 checker's `calleePinnedParams`);
  * that reader went with the bridge at A.11.7, leaving a fact nothing demanded, so only the configuration key remains
  * (docs/effects-as-rows.md A.11.9).
  */
object RunBoundaryFunctions {

  /** The set of run-boundary value FQNs a platform layer contributes in its `configure()` (the jvm plugin adds
    * `eliot.jvm::runMain`). `LangPlugin.initialize` reads it — all `configure()`s complete before any `initialize` —
    * and threads it to the row phase via `LangProcessors`, exactly as
    * [[com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding.extraNativeLabelsKey]] threads
    * native-contributor labels.
    */
  // Opaque to the cache identity: this set is a deterministic function of the active plugin set (fixed within a
  // compiler build, and covered by the compiler fingerprint) and the selected `main`, which already contributes.
  val configKey: Configuration.Key[Set[ValueFQN]] = Configuration.opaqueKey("runBoundaryFunctions")
}
