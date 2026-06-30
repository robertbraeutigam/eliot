package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** A module's ability surface, decoded from the qualifier-tagged names in its [[UnifiedModuleNames]]: the methods
  * declared inside its `ability` blocks (`Qualifier.Ability`) and the methods of the implementations it provides
  * (`Qualifier.AbilityImplementation`).
  *
  * Every implementation of an ability contributes one synthetic method per ability method, each carried as a
  * `Qualifier.AbilityImplementation(abilityName, index)` name; the method whose local name equals the ability name is
  * the implementation's "marker". Decoding these (and the `Qualifier.Ability` declarations) used to be open-coded — the
  * same `collect`-and-decode over the raw name map — in six places (`AbilityImplementationProcessor`,
  * `AbilityImplementationCheckProcessor`, `ImplementationMarkerUtils`, `MatchDesugarUtils`,
  * `ModuleAbilityOverlapCheckProcessor`). This fact owns that decode once; consumers query the structured lists instead
  * of re-pattern-matching the qualifier.
  *
  * @param declaredMethods
  *   One entry per method declared in an `ability` block in this module.
  * @param implementations
  *   One entry per ability-implementation method declared in the module, in name-table order.
  */
case class ModuleAbilities(
    moduleName: ModuleName,
    declaredMethods: Seq[ModuleAbilities.DeclaredMethod],
    implementations: Seq[ModuleAbilities.Impl],
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleAbilities] = ModuleAbilities.Key(moduleName, platform)

  /** The methods declared in the body of ability `abilityName` (its interface). */
  def declaredMethodsOf(abilityName: String): Seq[ValueFQN] =
    declaredMethods.filter(_.abilityName == abilityName).map(_.vfqn)

  /** Every method (of every implementation) of `abilityName` provided in this module. */
  def implementationMethodsOf(abilityName: String): Seq[ValueFQN] =
    implementations.filter(_.abilityName == abilityName).map(_.vfqn)

  /** The implementation methods named `methodName` belonging to an implementation of `abilityName`. */
  def namedImplementationMethodsOf(abilityName: String, methodName: String): Seq[ValueFQN] =
    implementations.filter(impl => impl.abilityName == abilityName && impl.methodName == methodName).map(_.vfqn)

  /** All marker methods (local name equal to the ability name) of implementations of `abilityName`. */
  def markersOf(abilityName: String): Seq[ValueFQN] =
    namedImplementationMethodsOf(abilityName, abilityName)

  /** The marker method of the implementation of `abilityName` at `index`, if present. */
  def markerOf(abilityName: String, index: Int): Option[ValueFQN] =
    implementations.collectFirst {
      case impl if impl.abilityName == abilityName && impl.methodName == abilityName && impl.index == index => impl.vfqn
    }
}

object ModuleAbilities {

  /** A method declared in the body of an `ability` block.
    *
    * @param vfqn
    *   The method's fully qualified name.
    * @param abilityName
    *   The declaring ability's local name.
    */
  case class DeclaredMethod(vfqn: ValueFQN, abilityName: String)

  /** A single ability-implementation method marker.
    *
    * @param vfqn
    *   The method's fully qualified name.
    * @param methodName
    *   The method's local name (equal to the ability name for the implementation's marker method).
    * @param abilityName
    *   The implemented ability's local name.
    * @param index
    *   The implementation index disambiguating multiple implementations of the same ability in one module.
    */
  case class Impl(vfqn: ValueFQN, methodName: String, abilityName: String, index: Int)

  case class Key(moduleName: ModuleName, platform: Platform = Platform.Runtime) extends CompilerFactKey[ModuleAbilities]
}
