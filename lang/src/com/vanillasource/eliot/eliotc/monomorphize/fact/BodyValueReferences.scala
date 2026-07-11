package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The value references appearing directly in a value's runtime body
  * ([[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.runtime]]) — empty for a body-less native, a
  * type constructor, or an abstract declaration.
  *
  * Memoizes the single body walk the checker would otherwise repeat on every evaluation: the
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker]]'s `ensureBinding` follows these transitively (the
  * binding cache short-circuits already-seen values, so cycles terminate) so that `renormalize` can re-fire a nested
  * stuck native once its bound metavariables solve — e.g. the `multiply`/`lessThanOrEqual`/`fold` inside a derived
  * `multiplyMin` used in the `*` result type. Platform-keyed like [[NativeBinding]], so each track walks its own bodies.
  *
  * @param vfqn
  *   the value whose checking body was walked
  * @param platform
  *   the source pool the body belongs to
  * @param references
  *   the value-reference FQNs the body names directly
  */
case class BodyValueReferences(
    vfqn: ValueFQN,
    platform: Platform,
    references: Set[ValueFQN]
) extends CompilerFact {
  override def key(): CompilerFactKey[BodyValueReferences] =
    BodyValueReferences.Key(vfqn, platform)
}

object BodyValueReferences {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[BodyValueReferences]
}
