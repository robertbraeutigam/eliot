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

/** What is left of carrier identification after effects v6: **one** structural predicate, "is this binder
  * higher-kinded?".
  *
  * There are no carriers any more — a row entry is a phantom binder of kind `Type` and a computation is a thunk — so
  * the readings that told a carrier binder from a bare higher-kinded generic, and a pinned stack from an open row, went
  * with them. What survives is the *kind* question, which is not about effects at all: `CarrierKindChecker` asks it to
  * reject an `[F[_]]` binder instantiated at a fully-applied proper type, and that is soundness (`docs/effects.md` §11,
  * "do not re-propose").
  */
object EffectCarriers {

  /** A generic binder is a carrier iff its kind is an arrow (`Type -> Type`, i.e. higher-kinded). */
  def isHktBinder(binder: SignatureView.Binder): Boolean =
    binder.parameterType.exists(pt => asArrow(pt.value).isDefined)

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
