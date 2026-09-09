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
  * **The contract with the desugar:** the binding is the **first ability-level type argument** of a method reference —
  * the ability's marker declares the phantom binder ahead of its pattern parameters, so a method reference carries
  * `[binding, pattern arguments…, method arguments…]` and the ability-level slice ([[AbilityResolver]]'s
  * `abilityArity`) *begins* with the binding.
  *
  * It is first, and not last as §10.1 step 6 originally fixed it, because F1 found that last cannot be written. A
  * type-argument list applies positionally, so writing an argument at index `k` means writing every argument before
  * it — and the pattern arguments of an ordinary ability call are exactly what no declaration determines. `show(x)`,
  * `a ++ b` and `sort(xs)` have their `Show[T]`/`Combine[T]`/`Ord[T]` arguments *inferred* by the checker today, and
  * the write cannot supply them without doing the inference itself, which rule 3 prohibits. With the binding first the
  * write is a one-element prefix, everything after it is inferred exactly as before, and the binding is never left to
  * a metavariable. The reversal is an encoding detail — nothing in §9 depends on which end it sits at — but it is a
  * reversal, and it is recorded here rather than made quietly.
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

  /** Split a reference's ability-level ground arguments into the binding and the pattern arguments, if the **first**
    * argument is a binding. `(args, None)` when it is not — the whole list is the pattern, which is every reference
    * of a tree whose abilities predate the phantom binder.
    */
  def split(abilityArgs: Seq[GroundValue]): (Seq[GroundValue], Option[ImplementationBinding]) =
    abilityArgs.headOption.flatMap(fromGround) match {
      case Some(binding) => (abilityArgs.tail, Some(binding))
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
