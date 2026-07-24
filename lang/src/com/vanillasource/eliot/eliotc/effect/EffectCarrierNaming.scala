package com.vanillasource.eliot.eliotc.effect

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

/** The single authority for the effect-ability ⇄ canonical-carrier correspondence — the `<Ability>Carrier` naming
  * convention that realizes an effect ability `E` as the concrete carrier type `ECarrier` colocated with it (so it
  * resolves wherever the ability does). Formerly a bare `abilityName + "Carrier"` string concat inlined in
  * [[com.vanillasource.eliot.eliotc.core.processor.EffectSugarDesugarer]]; extracted here so every reader of the
  * convention shares one definition:
  *
  *   - the **pinned-row desugar** (`{Throw[E] | G} A` ⤳ `ThrowCarrier[E, G, A]`),
  *   - the **row-argument type-pinning** rule (docs/effects-as-channel.md §10 U4-f — an open-row argument captured into
  *     a pinned-row parameter pins its ability arguments into the matching carrier layer's slots), and
  *   - later the §5 reify-legality check.
  *
  * The carrier of an effect ability carries the ability's non-carrier type arguments in its *leading* slots, then the
  * base carrier, then the result type (`Throw[E, F]` ⤳ `ThrowCarrier[E, base, result]`), so the base is always the
  * last spine element of the (result-unapplied) carrier value. The reverse direction (a ground carrier stack → its
  * surface row) is held by the LSP's `GroundValueRenderer`, which additionally needs each ability's argument count
  * without a signature at hand.
  */
object EffectCarrierNaming {

  /** The canonical carrier type name realizing an effect ability, by the `<Ability>Carrier` convention. */
  def carrierName(abilityName: String): String = abilityName + "Carrier"

  /** The canonical carrier type FQN realizing an effect ability — a type-namespace name colocated with the ability. */
  def carrierFQN(abilityFQN: AbilityFQN): ValueFQN =
    ValueFQN(abilityFQN.moduleName, QualifiedName(carrierName(abilityFQN.abilityName), Qualifier.Type))
}
