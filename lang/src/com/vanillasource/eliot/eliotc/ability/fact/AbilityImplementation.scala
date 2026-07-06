package com.vanillasource.eliot.eliotc.ability.fact

import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The outcome of resolving the ability implementation for `abilityValueFQN` at `typeArguments`, scoped to a source
  * [[Platform]]: the implementation (and its supporting markers/values) is searched in that platform's source pool.
  * [[Platform.Runtime]] is the default — every existing ability resolves under it — while the **compiler** pool carries
  * the compile-time carrier's `Effect`/`Throw[Either[String]]` instances, reachable only by querying under
  * [[Platform.Compiler]] (the ability-instance analogue of CP3's `CompilerNativesProcessor` for value bodies;
  * effectful-signatures W2a).
  *
  * The fact carries the [[AbilityImplementation.Resolution]] *outcome* rather than erroring in the producer: whether a
  * failed resolution is a compile error — and at what source position — is the *demander's* call, not the producer's.
  * The checker's saturation pass ([[com.vanillasource.eliot.eliotc.monomorphize.check.AbilityResolver]]) holds the
  * use-site reference and reports a failed demand there; the checker-inserted probes (`Coerce`, `Combine`, the
  * effect-lift arms) treat any non-[[AbilityImplementation.Resolution.Resolved]] outcome as a silent decline, since for
  * them non-applicability is a normal answer. An *absent* fact means the resolution aborted on an upstream error
  * (already reported at its own definition) — demanders stay silent on absence.
  */
case class AbilityImplementation(
    abilityValueFQN: ValueFQN,
    typeArguments: Seq[GroundValue],
    resolution: AbilityImplementation.Resolution,
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementation] =
    AbilityImplementation.Key(abilityValueFQN, typeArguments, platform)
}

object AbilityImplementation {

  /** How the ability resolution at one concrete type-argument tuple came out (the use-site side of ability-guards §3:
    * exactly-one-survivor after guard filtering).
    */
  enum Resolution {

    /** Exactly one implementation survived: the impl's method FQN and its own type parameters bound in declaration
      * order.
      */
    case Resolved(implementationFQN: ValueFQN, implementationTypeArgs: Seq[GroundValue])

    /** No implementation applies: nothing matched the pattern structurally, or every structural match declined via its
      * `where` guard.
      */
    case NoImplementation

    /** A `where` guard rejected this instantiation outright (`error(msg)` on the compile-time `Throw[String]`
      * channel), carrying the instance authors' messages — a hard error at every demanding use site.
      */
    case Rejected(messages: Seq[String])

    /** More than one implementation survived guard filtering — the use-site ambiguity of ability-guards §3. */
    case Ambiguous

    /** The resolved implementation, if this outcome is [[Resolved]] — the shape the resolve-and-use consumers (the
      * checker's `resolveAbility` callback) work with.
      */
    def resolved: Option[(ValueFQN, Seq[GroundValue])] = this match {
      case Resolved(implementationFQN, implementationTypeArgs) => Some((implementationFQN, implementationTypeArgs))
      case _                                                   => None
    }
  }

  case class Key(
      abilityValueFQN: ValueFQN,
      typeArguments: Seq[GroundValue],
      platform: Platform = Platform.Runtime
  ) extends CompilerFactKey[AbilityImplementation]
}
