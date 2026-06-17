package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.{
  ModuleName,
  QualifiedName,
  Qualifier,
  UnifiedModuleNames,
  UnifiedModuleValue,
  ValueFQN,
  WellKnownTypes
}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor

/** Emits [[NativeBinding]] facts for the abstract `match`-dispatch ability implementations, intercepting the FQNs that
  * [[UserValueNativesProcessor]] would otherwise bind to a body-less, useless [[VTopDef]]. Must be ordered ahead of
  * [[UserValueNativesProcessor]] in the processor sequence so it wins the fact for these FQNs (first registration wins).
  *
  *   - **`PatternMatch.handleCases`** (one impl per data type): a curried [[VNative]] collecting `(value, cases)`. The
  *     data type's value constructors, in source-declaration order, are baked in at fact-generation time. On a concrete
  *     `value` (a body-less constructor [[VTopDef]]), it computes the constructor's declaration index `i`, builds the
  *     Church selector `pickᵢ = \a0 … a_{N-1} -> a_i`, applies `cases` to it to obtain the matching handler, then
  *     applies that handler to the constructor's fields (or to a unit placeholder for a field-less constructor).
  *   - **`TypeMatch.typeMatch`** (one impl per constructor): a curried [[VNative]] collecting `(obj, matched,
  *     notMatched)`. The target type-constructor name is baked in. On a concrete `obj`, if its head matches the target
  *     it applies `matched` to `obj`'s fields, otherwise applies `notMatched` to a unit placeholder.
  *
  * On a symbolic (non-concrete) scrutinee the native stays stuck — open-term match dispatch is deferred (plan P4). All
  * dispatch is pure ([[SemValue]] `applyValue` only); the constructor metadata IO happens here, at fact-generation time.
  */
class MatchNativesProcessor extends SingleKeyTypeProcessor[NativeBinding.Key] {

  override protected def generateFact(key: NativeBinding.Key): CompilerIO[Unit] =
    if (WellKnownTypes.isPatternMatchHandleCases(key.vfqn)) buildHandleCases(key.vfqn).flatMap(registerFactIfClear)
    else if (WellKnownTypes.isTypeMatchTypeMatch(key.vfqn)) buildTypeMatch(key.vfqn).flatMap(registerFactIfClear)
    else abort

  private def buildHandleCases(vfqn: ValueFQN): CompilerIO[NativeBinding] =
    for {
      typeName <- requiredTypeName(vfqn, WellKnownTypes.patternMatchAbilityName)
      ordered  <- orderedConstructors(vfqn.moduleName, QualifiedName(typeName, Qualifier.Type))
    } yield NativeBinding(vfqn, handleCasesNative(ordered))

  private def buildTypeMatch(vfqn: ValueFQN): CompilerIO[NativeBinding] =
    requiredTypeName(vfqn, WellKnownTypes.typeMatchAbilityName).map(targetName =>
      NativeBinding(vfqn, typeMatchNative(targetName))
    )

  private def requiredTypeName(vfqn: ValueFQN, abilityName: String): CompilerIO[String] =
    ImplementationMarkerUtils.firstPatternTypeConstructorName(vfqn, abilityName).flatMap {
      case Some(name) => name.pure[CompilerIO]
      case None       => abort
    }

  /** The value constructors of a data type, in source-declaration order (= handler order).
    *
    * Reads [[RoleHint.ValueConstructor]] purely to recover constructor identity and declaration order while baking the
    * pattern-dispatch native — the native-emitting half of match desugaring. This is a sanctioned hint consumer (see the
    * cornerstone invariant on [[RoleHint]]): it reconstructs the programmer's written shape, makes no typing decision,
    * and never consults `typeParamCount`.
    */
  private def orderedConstructors(moduleName: ModuleName, dataType: QualifiedName): CompilerIO[Seq[ValueFQN]] =
    for {
      moduleNames     <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
      constructorVfqns = moduleNames.names.keys
                           .filter(qn => qn.qualifier == Qualifier.Default && qn.name.head.isUpper)
                           .map(qn => ValueFQN(moduleName, qn))
                           .toSeq
      ordered         <- constructorVfqns.traverseFilter { vfqn =>
                           getFactOrAbort(UnifiedModuleValue.Key(vfqn)).map { umv =>
                             umv.namedValue.roleHint match {
                               case RoleHint.ValueConstructor(dt, _) if dt == dataType =>
                                 Some((vfqn, umv.namedValue.qualifiedName.range.from))
                               case _                                                  => None
                             }
                           }
                         }
    } yield ordered.sortBy(_._2).map(_._1)

  /** `handleCases(value, cases)`: dispatch a concrete constructor `value` to its handler in `cases`. */
  private def handleCasesNative(ordered: Seq[ValueFQN]): SemValue =
    VNative(VType, value => VNative(VType, cases => dispatchHandleCases(ordered, value, cases)))

  private def dispatchHandleCases(ordered: Seq[ValueFQN], value: SemValue, cases: SemValue): SemValue =
    value match {
      case VTopDef(ctorFqn, None, spine) if ordered.contains(ctorFqn) =>
        val handler = Evaluator.applyValue(cases, churchSelector(ordered.size, ordered.indexOf(ctorFqn)))
        applyHandlerToFields(handler, spine.toList)
      case _                                                          => stuck(value)
    }

  /** `typeMatch(obj, matched, notMatched)`: if `obj`'s head is the target type constructor, run `matched` on its
    * fields, otherwise run `notMatched`.
    */
  private def typeMatchNative(targetName: String): SemValue =
    VNative(
      VType,
      obj => VNative(VType, matched => VNative(VType, notMatched => dispatchTypeMatch(targetName, obj, matched, notMatched)))
    )

  private def dispatchTypeMatch(targetName: String, obj: SemValue, matched: SemValue, notMatched: SemValue): SemValue =
    obj match {
      case VTopDef(headFqn, None, spine) if headFqn.name.name === targetName =>
        applyHandlerToFields(matched, spine.toList)
      case VTopDef(_, None, _)                                               =>
        Evaluator.applyValue(notMatched, unitValue)
      case _                                                                 => stuck(obj)
    }

  /** Apply a pattern handler to a constructor's fields. A field-less constructor's handler still takes one (ignored)
    * argument — the desugarer always emits a wildcard lambda — so it is applied to a unit placeholder.
    */
  private def applyHandlerToFields(handler: SemValue, fields: List[SemValue]): SemValue =
    if (fields.isEmpty) Evaluator.applyValue(handler, unitValue)
    else fields.foldLeft(handler)(Evaluator.applyValue)

  /** The Church selector `pickᵢ = \a0 … a_{n-1} -> a_i`, as nested [[VLam]]s. */
  private def churchSelector(n: Int, index: Int): SemValue = {
    def go(k: Int, collected: Vector[SemValue]): SemValue =
      if (k === n) collected(index)
      else VLam(s"$$case$k", arg => go(k + 1, collected :+ arg))
    go(0, Vector.empty)
  }

  /** Stuck dispatch on a symbolic scrutinee. Open-term re-firing is deferred (plan P4); for now this collapses to an
    * opaque neutral, mirroring [[Evaluator.applyValue]]'s `VNative`-on-neutral behaviour.
    */
  private def stuck(scrutinee: SemValue): SemValue =
    VNeutral(NeutralHead.VVar(0, "match"), Spine.SNil :+ scrutinee)

  /** Placeholder argument supplied to field-less constructor handlers (mirrors the JVM backend passing `null`). */
  private val unitValue: SemValue = VConst(GroundValue.Direct((), GroundValue.Type))
}
