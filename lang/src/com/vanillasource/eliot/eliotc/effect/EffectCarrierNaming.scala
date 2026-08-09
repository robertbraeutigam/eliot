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
  * base carrier, then the result type (`Throw[E, F]` ⤳ `ThrowCarrier[E, base, result]`), so the base is always the last
  * spine element of the (result-unapplied) carrier value. The reverse direction (a carrier stack → its surface row) is
  * [[abilityNameOfCarrier]] here, assembled into a row by [[com.vanillasource.eliot.eliotc.effect.EffectRowRendering]].
  */
object EffectCarrierNaming {

  private val carrierSuffix = "Carrier"

  /** The canonical carrier type name realizing an effect ability, by the `<Ability>Carrier` convention. */
  def carrierName(abilityName: String): String = abilityName + carrierSuffix

  /** The canonical carrier type FQN realizing an effect ability — a type-namespace name colocated with the ability. */
  def carrierFQN(abilityFQN: AbilityFQN): ValueFQN =
    ValueFQN(abilityFQN.moduleName, QualifiedName(carrierName(abilityFQN.abilityName), Qualifier.Type))

  /** The inverse of [[carrierName]]: the effect ability a type FQN is the canonical carrier of, or [[None]] if it does
    * not follow the convention. Both halves of the convention are required — the name must be `<Ability>Carrier` *and*
    * it must be colocated with that ability, i.e. live in the module the ability names (`ThrowCarrier` in
    * `eliot.effect.Throw`). Requiring colocation costs nothing (the forward direction builds exactly that FQN) and
    * makes a coincidental `FooCarrier` in an unrelated module a non-match.
    *
    * '''Rendering only.''' This inverse exists so a carrier stack can be shown to a user as the pinned row that spells
    * it, instead of leaking machinery names. It must **never** drive a typing, routing or elaboration decision:
    * recognizing carrier-ness by name miscompiles in both directions, and the sanctioned signal there is the
    * elaboration-threaded tag (`EffectRow.pinnedParameterIndices` / `RunBoundaryFunctions`) — see
    * docs/effects-as-channel.md finding 14 and the *Effects Are a Channel* cornerstone. A wrong answer here is a
    * cosmetic misrendering; a wrong answer in the checker is a miscompile.
    */
  def abilityNameOfCarrier(fqn: ValueFQN): Option[String] = {
    val name = fqn.name.name
    Option
      .when(name.length > carrierSuffix.length && name.endsWith(carrierSuffix))(name.dropRight(carrierSuffix.length))
      .filter(_ == fqn.moduleName.name)
  }
}
