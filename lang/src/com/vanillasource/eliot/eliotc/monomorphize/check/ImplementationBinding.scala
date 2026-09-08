package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue

/** The value of a **phantom binding binder** (effects v6, `docs/effects.md` §9.4): what a row entry's or a `~`
  * constraint's compile-time parameter carries at a reference, read back from the reference's ground type arguments.
  *
  * A binder is written by the desugar at every reference, never inferred, and holds one of two ground values:
  *   - the [[WellKnownTypes.defaultImplementationFQN]] sentinel — [[Default]], "search at the ground arguments", the
  *     two-site resolution every reference gets today;
  *   - an implementation, headed by a [[Qualifier.AbilityImplementation]]-qualified name and applied to that
  *     implementation's own type arguments in declaration order — [[Implementation]], used **directly**: no candidate
  *     search, no `where` guard, no coherence question (§9.4 step 4).
  *
  * **The contract with the desugar (§10.1 step 6):** the binding is the **last ability-level type argument** of a
  * method reference — the ability's marker declares its pattern parameters followed by the phantom binder, so a method
  * reference carries `[pattern arguments…, binding, method arguments…]`, and the ability-level slice
  * ([[AbilityResolver]]'s `abilityArity`) ends with the binding. Today no marker declares a phantom binder, so no
  * reference carries one and the slice is exactly what it was; the reader answers [[None]] for every argument list of
  * the tree as it stands.
  *
  * Recognition is by two compiler-owned tags and nothing else: the sentinel's FQN, and an implementation's **marker** —
  * the name in an implementation's namespace whose local name is the ability's own (`Console@Console#…`), which is what
  * the desugar writes. The marker is required, not merely the namespace, because an implementation's namespace also
  * holds its associated types (`type AddResult = …` inside an `implement` block), and an associated type *is* a type
  * that can occupy a pattern slot; its local name is its own, never the ability's, so the marker test keeps the read
  * exact rather than a naming convention. The sentinel is declared nowhere, so it can occupy no slot in any other role.
  */
enum ImplementationBinding {

  /** Search at the ground pattern arguments — today's two-site resolution. */
  case Default

  /** A fixed implementation: `head` is the implementation's marker, and `typeArguments` the implementation's own type
    * arguments in declaration order — exactly what
    * [[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation.Resolution.Resolved]] carries.
    */
  case Implementation(head: ValueFQN, typeArguments: Seq[GroundValue])
}

object ImplementationBinding {

  /** The ground form of [[Default]]. */
  val defaultGround: GroundValue =
    GroundValue.Structure(WellKnownTypes.defaultImplementationFQN, Seq.empty, GroundValue.Type)

  /** Read one ground type argument as a binding, [[None]] when it is an ordinary type argument. */
  def fromGround(value: GroundValue): Option[ImplementationBinding] =
    value match {
      case GroundValue.Structure(name, Seq(), _) if name === WellKnownTypes.defaultImplementationFQN =>
        Some(Default)
      case GroundValue.Structure(name, args, _)                                                   =>
        name.name.qualifier match {
          case Qualifier.AbilityImplementation(abilityName, _, _) if name.name.name == abilityName =>
            Some(Implementation(name, args))
          case _                                                                                  => None
        }
      case _                                                                                      => None
    }

  /** Split a reference's ability-level ground arguments into the pattern arguments and the binding, if the last
    * argument is one. `(args, None)` when it is not — the whole list is the pattern.
    */
  def split(abilityArgs: Seq[GroundValue]): (Seq[GroundValue], Option[ImplementationBinding]) =
    abilityArgs.lastOption.flatMap(fromGround) match {
      case Some(binding) => (abilityArgs.init, Some(binding))
      case None          => (abilityArgs, None)
    }

  extension (self: Implementation)

    /** The method `methodName` of this implementation: the implementation's namespace is the qualifier its head
      * carries, and its module is the head's, so the method is that name in the same module and namespace —
      * the same construction `AbilityImplementationProcessor.markerVfqnFor` runs in the other direction.
      */
    def method(methodName: String): ValueFQN =
      ValueFQN(self.head.moduleName, QualifiedName(methodName, self.head.name.qualifier))
}
