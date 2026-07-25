package com.vanillasource.eliot.eliotc.effect.processor

import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN}

/** Recognition of the internal effect machinery. The user never imports or names it: the `Effect`/`Suspend` abilities
  * are inserted by the compiler (their `flatMap`/`pure`/`map` references are spliced by fully-qualified name — see
  * [[com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.effectFlatMapFQN]] — by the checker's
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]]) and monomorphization pins the carrier and
  * erases the whole tower. [[isMachineryAbility]] tells the monomorphize-phase effect accounting
  * ([[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccountingProcessor]]) that an `Effect`/`Suspend` call
  * is compiler machinery, so it never counts as a user-facing effect. The construction half (the former
  * `pureWrap`/`sequence` ORE builders) moved into the lifter with the auto-lift itself.
  */
object EffectMachinery {

  /** The machinery ability a *side-effecting* action rides: every fine effect that actually touches the world
    * (`Console`, `Log`, `FileSystem`) is implemented over `Suspend`, so a demand for it is the compiler asking "can
    * this carrier perform a side effect?". Notably the identity carrier `Id` has no `Suspend` instance *by design*
    * (docs/effects-as-channel.md §6), which is what keeps I/O out of a pure computation.
    */
  val suspendAbilityName: String = "Suspend"

  /** The abilities the compiler inserts and recognises but the user never names. */
  private val machineryAbilities: Set[String] = Set("Effect", suspendAbilityName)

  /** The internal effect machinery, never a user-facing effect: a `flatMap`/`pure`/`map`/`suspend` call (hand-written or
    * inserted by this phase) must not be counted as "using an effect" by the declared-effect check.
    */
  def isMachineryAbility(abilityName: String): Boolean = machineryAbilities.contains(abilityName)

  /** The ability a value reference belongs to, if it is an ability method (`printLine` → `Console`, `flatMap` → `Effect`);
    * `None` for an ordinary (non-ability) value. Lets callers ask "which ability does this call name?" without
    * re-matching on [[Qualifier]].
    */
  def abilityNameOf(fqn: ValueFQN): Option[String] =
    fqn.name.qualifier match {
      case Qualifier.Ability(name) => Some(name)
      case _                       => None
    }
}
