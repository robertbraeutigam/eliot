package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{
  boolFQN,
  boolFalseFQN,
  boolFoldFQN,
  boolTrueFQN,
  functionDataTypeFQN,
  typeFQN
}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.UpToDate
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{BindingContribution, ContributedBinding, GroundValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** The `system` native contributor: emits the total [[ContributedBinding]] under [[ContributedBinding.systemLabel]] for
  * the language-intrinsic system values the compiler itself reasons about — Function (type constructor), Type, and the
  * compile-time Bool primitives `true`/`false`/`fold` — and `None` for every other name.
  *
  * Function is wired as a curried native that takes two type args (A, B) and produces VPi(A, _ => B): the Π-former is
  * the single primitive type former, so every function type is a VPi (read back to a Function structure only at quote
  * time by the Quoter).
  *
  * Bool is declared opaque in the language (`type Bool`); its compile-time representation is supplied here as
  * `VConst(Direct(Boolean, …))` so type-level predicates reduce during checking, and `fold` (the only way to branch on
  * an opaque `Bool`, which the checker collapses at compile time via `PostDrainQuoter`) selects a branch when its
  * condition is concrete. Library Bool/BigInteger operations whose reduction the compiler merely supplies but does not
  * reason about (`&&`, `lessThanOrEqual`, the arithmetic natives backing `Int`'s dependent bounds) live in the stdlib
  * layer's `StdlibNativesProcessor`, not here.
  *
  * The system names are disjoint from every other native supplier (the [[BindingMergerProcessor]] relies on native
  * disjointness): `Type` is owned here, not by `DataTypeNativesProcessor` (which excludes it).
  */
class SystemNativesProcessor extends SingleFactProcessor[ContributedBinding.Key] {

  private val boolType: SemValue = VTopDef(boolFQN, None, Spine.SNil)

  /** The canonical stuck form of a native: a [[VStuckNative]] carrying the native's own FQN and the (not-yet-concrete)
    * arguments as its spine. Keeping the FQN is what lets distinct stuck natives stay definitionally distinct and lets
    * [[Evaluator.renormalize]] re-fire them once the arguments become concrete; the dedicated [[VStuckNative]] head
    * (rather than a body-less [[VTopDef]]) keeps the non-injective native from being injectivity-decomposed by the
    * unifier or read back as a ground type by the quoter.
    */
  private def stuck(fqn: ValueFQN, args: SemValue*): SemValue =
    VStuckNative(fqn, args.foldLeft(Spine.SNil: Spine)(_ :+ _))

  override def generateSingleFact(key: ContributedBinding.Key): CompilerIO[ContributedBinding] =
    if (key.label =!= ContributedBinding.systemLabel) abort
    // The system reductions are input-less compiler constants. Depending on the always-clean `UpToDate` leaf (the value
    // is immaterial — only the edge matters) lets the incremental cache prove them unchanged on a no-change run instead
    // of treating them as source leaves and regenerating them every time. Deliberately not `getFactOrAbort`: a bundle
    // without `UpToDateProcessor` (e.g. a minimal test) just loses incrementality here, never fails.
    else
      getFact(UpToDate.Key()) >>
        ContributedBinding(key.vfqn, key.label, systemReduction(key.vfqn).map(BindingContribution.Leaf(_))).pure[CompilerIO]

  /** The host-runnable reduction for a system name, or `None` if `vfqn` is not a system name (totality). */
  private def systemReduction(vfqn: ValueFQN): Option[SemValue] =
    if (vfqn === functionDataTypeFQN) functionNative.some
    else if (vfqn === typeFQN) VType.some
    else if (vfqn === boolTrueFQN) Evaluator.trueValue.some
    else if (vfqn === boolFalseFQN) Evaluator.falseValue.some
    else if (vfqn === boolFoldFQN) boolFoldNative.some
    else none

  /** Function[A, B] is a curried native: first takes A (domain), then B (codomain), and produces VPi(A, _ => B). */
  private def functionNative: SemValue =
    VNative(
      VType,
      domain => VNative(VType, codomain => VPi(domain, _ => codomain))
    )

  /** `fold(condition, whenTrue, whenFalse)`: selects a branch when the condition is a concrete Bool, otherwise stays
    * stuck. The type parameter `A` is implicit (never applied at evaluation time, since implicit type args are not
    * threaded into the ORE), so the native takes exactly the three value arguments. The branches are passed through
    * unevaluated-by-selection — NbE has already evaluated them to SemValues, but only the chosen one is returned.
    */
  private def boolFoldNative: SemValue =
    VNative(boolType, cond => VNative(VType, whenTrue => VNative(VType, whenFalse => foldResult(cond, whenTrue, whenFalse))))

  private def foldResult(cond: SemValue, whenTrue: SemValue, whenFalse: SemValue): SemValue = cond match {
    case VConst(GroundValue.Direct(true, _))  => whenTrue
    case VConst(GroundValue.Direct(false, _)) => whenFalse
    case _                                    => stuck(boolFoldFQN, cond, whenTrue, whenFalse)
  }
}
