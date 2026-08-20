package com.vanillasource.eliot.eliotc.effect.processor

import com.vanillasource.eliot.eliotc.module.fact.Qualifier
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  FunctionApplication,
  FunctionLiteral,
  ParameterReference,
  SignatureView,
  ValueReference,
  asArrow
}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityConstraint, AbilityFQN}

/** Identifying effect carriers. An effect carrier is a value's higher-kinded generic binder (`F` in `[F[_] ~ E...]`,
  * the shape the M1 `{E...}` sugar produces) that an effectful result rides in (`F[String]`, `IO[Unit]`). The
  * predicates here are the structural tests the rest of the pass uses to find carriers, to read off the effects they
  * declare, and to decide whether a type is effectful (carrier-headed).
  */
object EffectCarriers {

  /** A generic binder is a carrier iff its kind is an arrow (`Type -> Type`, i.e. higher-kinded). */
  def isHktBinder(binder: SignatureView.Binder): Boolean =
    binder.parameterType.exists(pt => asArrow(pt.value).isDefined)

  /** The higher-kinded binder names of a signature — the binders an effectful result can ride (`F` in `F[Unit]`). This
    * is the *callee* notion of a carrier: an ability method's return rides its ability's binder (`printLine : F[Unit]`)
    * even though that binder carries no constraint on the method itself, so no constraint is required here. A *value's
    * own* ambient effect carriers are the further-filtered subset whose binder is ability-constrained — the
    * `carrierBinders ∩ paramConstraints` filter the monomorphize-phase effect accounting applies (see
    * [[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccountingProcessor]]), which excludes a bare generic
    * `C[_, _]`.
    */
  def carrierBinders(view: SignatureView): Set[String] =
    view.binders.filter(isHktBinder).map(_.name.value).toSet

  /** A definition's **declared** carrier binders: the higher-kinded binders that the definition's own signature says
    * carry effects, as opposed to a bare higher-kinded generic (`def id[F[_]](x: F[A]): F[A]`, `data Box[A]`'s
    * constructor-class abstractions) which is an ordinary type-constructor parameter and carries nothing.
    *
    * Where [[carrierBinders]] answers "which binder *could* a computation ride" — the shape question, used where the
    * carrier is known from context — this answers "which binder *does* this signature declare as a carrier", which is
    * what a phase reading only declarations (the effects-as-rows elaboration) must ask before deciding that a call is a
    * computation. A binder qualifies when:
    *
    *   - it is **ability-constrained** (`[G[_] ~ Effect]`, and every binder the `{E}` effect-row sugar mints — an
    *     ability method's own binder included, since its rows desugar onto exactly that binder);
    *   - it heads a **pinned row** the signature discharges (`runAbort[G[_], A](obj: {Abort | G} A): G[Option[A]]` —
    *     the base carrier of a pinned stack is deliberately unconstrained, so nothing else marks it).
    *
    *   - it is a **machinery** ability's method (`Effect`'s `pure`/`flatMap`/`map`, `Suspend`'s `suspend`), whose
    *     binder *is* the carrier by construction. These are the one kind of ability method that cannot say so with a
    *     row: machinery entries are filtered out of every row by design (they are compiler-inserted, never a
    *     user-facing effect), so `{Effect}` on `pure` would say nothing. They are recognised by name, which is where
    *     [[EffectMachinery.isMachineryAbility]] is already the sanctioned recogniser.
    *
    * **A user ability method has no rule of its own.** It used to: every higher-kinded binder of an ability method
    * counted as a carrier, on the reasoning that `Console[F[_]]` and a constructor-class `Container[F[_]]` are the same
    * shape and the *use site* separates them. It does not — the use site is never consulted by the row derivation, so a
    * constructor class was read as an effect and `def unboxed(b: Box[String]): String = unwrap(b)` was rejected as
    * "performs the effect 'Container'". An ability method now declares its effects the way every other definition does,
    * with a row (`def printLine(s: String): {Console} Unit`), and the general constraint rule above answers for it. A
    * method that declares no row performs nothing, which is exactly what a constructor class is.
    */
  def declaredCarrierBinders(value: OperatorResolvedValue): Set[String] = {
    val view       = SignatureView.of(value.signature)
    val allBinders = carrierBinders(view)
    if (isMachineryMethod(value)) allBinders
    else {
      val pinnedTypes =
        value.effectRow.pinnedParameterIndices.toSeq.flatMap(index => view.parameters.lift(index).map(_.value)) ++
          Option.when(value.effectRow.returnPinnedEffects.nonEmpty)(view.returnType.value)
      allBinders.filter(binder =>
        value.paramConstraints.get(binder).exists(_.nonEmpty) ||
          pinnedTypes.exists(occurs(binder, _))
      )
    }
  }

  /** Whether a value is a method of one of the compiler's own machinery abilities (`Effect`/`Suspend`). */
  private def isMachineryMethod(value: OperatorResolvedValue): Boolean =
    value.vfqn.name.qualifier match {
      case Qualifier.Ability(name) => EffectMachinery.isMachineryAbility(name)
      case _                       => false
    }

  /** Whether a binder name occurs anywhere in a type expression. */
  private def occurs(binder: String, tpe: OperatorResolvedExpression): Boolean = tpe match {
    case ParameterReference(name)            => name.value == binder
    case FunctionApplication(target, arg)    => occurs(binder, target.value) || occurs(binder, arg.value)
    case FunctionLiteral(_, paramType, body) =>
      paramType.exists(pt => occurs(binder, pt.value)) || occurs(binder, body.value)
    case ValueReference(_, typeArgs)         => typeArgs.exists(ta => occurs(binder, ta.value))
    case _                                   => false
  }

  /** The user-facing effects a value *declares*: the ability FQNs constrained on its `carriers`, with the internal
    * machinery abilities (`Effect`/`Suspend`) removed — those are inserted by the compiler, never declared as effects.
    * This is both a callee's propagated effect set and the declared set the subset check honours.
    */
  def declaredEffects(
      carriers: Set[String],
      paramConstraints: Map[String, Seq[AbilityConstraint[OperatorResolvedExpression]]]
  ): Set[AbilityFQN] =
    carriers
      .flatMap(c => paramConstraints.getOrElse(c, Seq.empty).map(_.abilityFQN))
      .filterNot(a => EffectMachinery.isMachineryAbility(a.abilityName))
}
