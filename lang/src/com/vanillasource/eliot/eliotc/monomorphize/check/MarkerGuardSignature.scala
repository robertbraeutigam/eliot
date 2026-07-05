package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.data.NonEmptySeq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.resolve.fact.Qualifier
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Rewrites an ability-implementation *marker* value so its `where` guard can be monomorphized.
  *
  * A marker is a synthetic value whose local name equals the ability's name and whose curried argument types encode the
  * implementation's head pattern; its return-type slot carries the `where` guard (ability-guards §2.3). The pattern
  * arguments are what `AbilityMatcher` unifies at a call site — they are *not* ordinary value parameters and need not be
  * of kind `Type` (a higher-kinded ability argument like `Throw[E2, ThrowCarrier[E1, G]]` puts a `Type -> Type` carrier
  * in an argument position). Monomorphizing the marker to read its guard would run the full kind checker over those
  * argument types and reject the higher-kinded ones.
  *
  * Since the guard verdict depends only on the marker's leading type-parameter *binders* (the pattern arguments are
  * irrelevant to it), the guard discharge monomorphizes a **parameter-stripped** view: the binders and the guard return,
  * with the pattern-argument arrows dropped. That view is well-kinded (binders + a `Bool`-valued return) and reduces the
  * guard exactly, without ever kind-checking the pattern.
  */
object MarkerGuardSignature {

  /** If `resolvedValue` is an ability-implementation marker, return a copy whose signature keeps only the leading
    * generic binders and the guard return (pattern-argument arrows dropped); otherwise return it unchanged.
    */
  def strippedForGuard(resolvedValue: OperatorResolvedValue): OperatorResolvedValue =
    if (isMarker(resolvedValue)) resolvedValue.copy(typeStack = stripSignature(resolvedValue.typeStack))
    else resolvedValue

  /** A marker's local name equals its ability's name (methods carry the method name); its qualifier is an
    * [[Qualifier.AbilityImplementation]].
    */
  private def isMarker(resolvedValue: OperatorResolvedValue): Boolean =
    resolvedValue.name.value.qualifier match {
      case Qualifier.AbilityImplementation(abilityFQN, _) => resolvedValue.vfqn.name.name == abilityFQN.abilityName
      case _                                              => false
    }

  private def stripSignature(
      typeStack: Sourced[TypeStack[OperatorResolvedExpression]]
  ): Sourced[TypeStack[OperatorResolvedExpression]] = {
    // Only the bottom level (index 0) is the value's signature; the higher levels are its kind chain and are irrelevant
    // to the guard, which is read off the (stripped) signature's return.
    val levels   = typeStack.value.levels
    val view     = SignatureView.of(typeStack.as(levels.head))
    val stripped = view.withParameters(Seq.empty).toExpression
    typeStack.as(TypeStack(NonEmptySeq(stripped, levels.tail)))
  }
}
