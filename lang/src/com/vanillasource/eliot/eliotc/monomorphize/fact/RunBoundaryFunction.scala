package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.plugin.Configuration
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** Marks a value as a platform **run boundary** whose sole carrier parameter (index 0) is a carrier **capture** — the
  * effects-as-channel carrier-stack recognition tag, **source (ii)** (docs/effects-as-channel.md §7 step 4 / finding
  * 14). The one instance today is the jvm run boundary `eliot.jvm::runMain` (`def runMain[A](io: IO[A]): A`), where the
  * user `main`'s inferable carrier `?F[Unit]` binds to the concrete platform carrier `IO`.
  *
  * This is the platform-contributed counterpart of the pinned-row desugar tag ([[com.vanillasource.eliot.eliotc.ast.fact.EffectRow.pinnedParameterIndices]],
  * source (i)): a *concrete* carrier stack like `IO[A]` cannot be spelled as a pinned row (`IO` is a `Suspend`-riding
  * carrier with no ability, and is jvm-owned so lang must never name it), yet the checker still needs to recognise it as
  * a carrier slot rather than a data container — which shape/name detection cannot do soundly (finding 14). The platform
  * that owns the carrier declares the boundary here, by construction; the checker
  * ([[com.vanillasource.eliot.eliotc.monomorphize.check.Checker.calleePinnedParams]]) reads this fact generically and
  * routes the capture through the join solver, never guessing carrier-ness from the domain's shape.
  *
  * Produced by [[com.vanillasource.eliot.eliotc.monomorphize.processor.RunBoundaryFunctionProcessor]] for exactly the
  * FQNs a layer registers in [[RunBoundaryFunction.configKey]] (the jvm plugin adds `runMain`); it declines for every
  * other FQN, so a value that is not a registered boundary carries no tag.
  *
  * @param vfqn
  *   The run-boundary value's fully qualified name.
  */
case class RunBoundaryFunction(vfqn: ValueFQN) extends CompilerFact {
  override def key(): CompilerFactKey[RunBoundaryFunction] = RunBoundaryFunction.Key(vfqn)
}

object RunBoundaryFunction {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[RunBoundaryFunction]

  /** The set of run-boundary value FQNs a platform layer contributes in its `configure()` (the jvm plugin adds
    * `eliot.jvm::runMain`). `LangPlugin.initialize` reads it — all `configure()`s complete before any `initialize` — and
    * threads it to [[com.vanillasource.eliot.eliotc.monomorphize.processor.RunBoundaryFunctionProcessor]] via
    * `LangProcessors`, exactly as [[ContributedBinding.extraNativeLabelsKey]] threads native-contributor labels. Empty
    * for a lang-only build (the tag processor then declines every FQN, so the checker sees no boundaries).
    */
  val configKey: Configuration.Key[Set[ValueFQN]] = Configuration.namedKey("runBoundaryFunctions")
}
