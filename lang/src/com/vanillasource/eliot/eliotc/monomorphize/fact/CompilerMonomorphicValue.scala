package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A compiler-platform monomorphized value: the compiler track's analogue of [[MonomorphicValue]].
  *
  * It is produced by
  * [[com.vanillasource.eliot.eliotc.monomorphize.processor.CompilerMonomorphicTypeCheckProcessor]] by running the same
  * checker in the `Platform.Compiler` source pool, so its bodies, ability instances, and native leaves come from the
  * compiler layer (never the runtime target). It is a **distinct fact type** from [[MonomorphicValue]] on purpose: the
  * compiler processor cannot even name `MonomorphicValue.Key`, so the `compiler-mono → runtime-mono` edge is impossible
  * by construction and acyclicity is enforced rather than merely disciplined.
  *
  * It is keyed per-instantiation, exactly like [[MonomorphicValue]]: the same value reduced at different type arguments
  * is a different fact. A compile-time ability dispatched on a generic parameter can only reduce once that generic is
  * concrete, so the runtime track requests this fact at the concrete type arguments of a use site.
  *
  * @param vfqn
  *   The fully qualified name of the original value
  * @param typeArguments
  *   The concrete type arguments used for specialization
  * @param name
  *   The sourced name of the value
  * @param signature
  *   The concrete ground type of this specialized instance, checked in the compiler platform
  * @param reduced
  *   The optional reduced compile-time body — the compiler track's analogue of [[MonomorphicValue.runtime]]. It is not
  *   destined for a code generator; it is the reduced value the runtime track's evaluator plugs in as a native.
  * @param typeLevel
  *   The *type level* of the value reduced (the type-levels-as-values plan): 0 = the ordinary value, `n ≥ 1` = its
  *   level-`n` type expression. For a level-`n` value, `reduced` carries the value's instantiated level-`n` expression
  *   (e.g. the reduced signature at `typeArguments`), and `signature` its kind.
  */
case class CompilerMonomorphicValue(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue],
    name: Sourced[QualifiedName],
    signature: GroundValue,
    reduced: Option[Sourced[MonomorphicExpression.Expression]],
    typeLevel: Int = 0
) extends CompilerFact {
  override def key(): CompilerFactKey[CompilerMonomorphicValue] =
    CompilerMonomorphicValue.Key(vfqn, typeArguments, typeLevel)
}

object CompilerMonomorphicValue {

  /** Composite key mirroring [[MonomorphicValue.Key]]: same value, different type arguments → different key. The
    * `typeLevel` dimension additionally distinguishes a value's level-`n` type expression from its level-0 self; a
    * level `≥ 1` key is answered by deriving the level value's [[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue]]
    * and running the same checker (`MonomorphicValue.Key` stays level-0-only — level `≥ 1` is compiler-track by
    * definition).
    */
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue], typeLevel: Int = 0)
      extends CompilerFactKey[CompilerMonomorphicValue]
}
