package com.vanillasource.eliot.eliotc.saturate.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** An [[OperatorResolvedValue]] whose source type stack has been *saturated*: every parameter-position bare reference
  * to an omittable (`auto`-marked) type constructor — e.g. a bare `Int` where `Int[MIN, MAX]` was meant — has been
  * rewritten into an explicit application over fresh generic binders, and those binders prepended to the value's own
  * generic prefix (implicit-generics, W1). The synthesized binders are ordinary generic parameters, so the existing
  * "too few explicit type args → infer the rest" machinery solves them from each call's arguments with no new
  * inference.
  *
  * This is the fact every checker-phase reader of a value's *type stack* consumes instead of [[OperatorResolvedValue]]
  * (the monomorphize entry point and the `ValueReference` read in `Checker`). We cannot rewrite
  * [[OperatorResolvedValue]] in place — it is produced upstream by the `operator` phase, so feeding the rewrite back
  * would be a fact cycle.
  *
  * Return-position bare references are left untouched for W1 (they are *calculated*, handled in W3); a value with no
  * parameter-position bare omittable reference carries its [[OperatorResolvedValue]] through unchanged.
  *
  * @param value
  *   The (possibly rewritten) operator-resolved value, carrying the saturated type stack.
  * @param typeLevel
  *   The *type level* of this value (the type-levels-as-values plan). Level 0 is the ordinary runtime-body value
  *   (produced by `SaturatedValueProcessor`). Level `n ≥ 1` is a *type expression* run as a named value on the compiler
  *   track — its body is the host value's `TypeStack.levels(n-1)`, its signature the level above (see
  *   `TypeLevelSaturatedValueProcessor`). A level value is an ordinary [[OperatorResolvedValue]]; the level only
  *   distinguishes the *key* so the derivation and the host coexist without a synthetic FQN.
  */
case class SaturatedValue(value: OperatorResolvedValue, typeLevel: Int = 0) extends CompilerFact {
  override def key(): CompilerFactKey[SaturatedValue] = SaturatedValue.Key(value.vfqn, value.platform, typeLevel)

  /** The codegen-relevance classification of each leading type-stack binder, computed once on this saturated value —
    * the monomorphization-keying plan's B1 analysis (grown from the D6 reified-binder flag). Each binder carries its
    * reified/dispatched/representation roles and a derived [[BinderRoles.Disposition]]. Two consumers use it: the
    * monomorphize binding cache (`BindingClosure.reifyingWrap`, via [[BinderRoles.reifiedPrefixBinders]]) and the
    * codegen-key dedup (`used/CodegenProjection.scala`). Computed on the *saturated* signature so the binder indices
    * align with the type arguments the checker applies.
    */
  lazy val binderRoles: BinderRoles = BinderRoles.of(value)
}

object SaturatedValue {

  /** @param typeLevel
    *   0 = the ordinary runtime-body value; `n ≥ 1` = the level-`n` type expression run as a named value on the
    *   compiler track. A proper key dimension (not a mangled FQN): level identity is structural.
    */
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime, typeLevel: Int = 0)
      extends CompilerFactKey[SaturatedValue]
}
