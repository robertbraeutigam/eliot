package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{
  bigIntFQN,
  boolFQN,
  boolFalseFQN,
  boolFoldFQN,
  boolTrueFQN,
  functionDataTypeFQN,
  integerLiteralFQN,
  typeEqualsFQN,
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
  * the language-intrinsic system values the compiler itself reasons about — Function (type constructor), Type, the
  * compile-time Bool primitives `true`/`false`/`fold`, the `Eq[Type]` structural-equality leaf `typeEquals`, and the
  * value-position literal protocol `integerLiteral` — and `None` for every other name.
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
  * `typeEquals` compares two type normal forms and reads the answer back as a `Bool` — definitional type equality made a
  * first-class value, backing the compiler-pool `Eq[Type]` instance (`compiler/.../Eq.els`). Like `fold` it is a
  * compiler intrinsic the checker reasons about (a guard `where E1 != E2` runs it during ability resolution), so it is
  * owned here rather than in the library layer.
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
    // of treating them as source leaves and regenerating them every time. Deliberately tolerant (`getFactIfProduced`):
    // a bundle without `UpToDateProcessor` (e.g. a minimal test) just loses incrementality here, never fails.
    else
      getFactIfProduced(UpToDate.Key()) >>
        ContributedBinding(key.vfqn, key.label, systemReduction(key.vfqn).map(BindingContribution.Leaf(_))).pure[CompilerIO]

  /** The host-runnable reduction for a system name, or `None` if `vfqn` is not a system name (totality). */
  private def systemReduction(vfqn: ValueFQN): Option[SemValue] =
    if (vfqn === functionDataTypeFQN) functionNative.some
    else if (vfqn === typeFQN) VType.some
    else if (vfqn === boolTrueFQN) Evaluator.trueValue.some
    else if (vfqn === boolFalseFQN) Evaluator.falseValue.some
    else if (vfqn === boolFoldFQN) boolFoldNative.some
    else if (vfqn === typeEqualsFQN) typeEqualsNative.some
    else if (vfqn === integerLiteralFQN) integerLiteralNative.some
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

  /** `integerLiteral[V]: IntegerLiteralType[V]` — the value-position literal protocol. A value-position literal `n` is
    * desugared to `integerLiteral[n]` so the checker types it as the platform singleton `IntegerLiteralType[n]`
    * (`CoreExpressionConverter`; see [[WellKnownTypes.integerLiteralFQN]]). The backend reads it back by a *quote-time
    * rewrite* to a plain `IntegerLiteral` node ([[com.vanillasource.eliot.eliotc.monomorphize.check.PostDrainQuoter]]) —
    * so a value body's literal is never evaluated and stays structural for codegen.
    *
    * But when the literal is *consumed by a compile-time computation* — a `where` guard bound `fitsIn[byteMin, …]`, or
    * any type-level use where its `BigInteger` value feeds a native like `lessThanOrEqual` — the rewrite is too late: the
    * consuming native must fire during *evaluation*, and an unreduced `integerLiteral[V]` argument leaves it stuck. So the
    * evaluator reduction is its arithmetic value: `integerLiteral[V]` reduces to `V`. This never disturbs codegen, where
    * the body is quoted structurally (the native is not consulted) and the rewrite still applies.
    */
  private def integerLiteralNative: SemValue =
    VNative(VTopDef(bigIntFQN, None, Spine.SNil), v => v)

  /** `typeEquals(a: Type, b: Type): Bool` — the `Eq[Type]` leaf. Compares two types by their normal forms, which (since
    * everything is forced/normalised first) is exactly the compiler's one notion of definitional equality read back as a
    * `Bool`. Concrete-only, and fail-safe: it reduces to `true`/`false` only when both arguments are fully concrete
    * normal forms, and otherwise stays stuck (a [[VStuckNative]]) rather than answering wrongly — so a non-concrete
    * argument hard-errors at read-back instead of comparing a `Type`-collapsed placeholder equal. At a real use site
    * arguments are always ground (`AbilityResolver` quotes before resolving), so the stuck case does not arise there.
    *
    * Pure structural comparison, mirroring `Unifier.groundEquals`; it never goes through the [[Unifier]], whose equality
    * solves metas as a side effect (an equality *test* must not mutate the meta store).
    */
  private def typeEqualsNative: SemValue =
    VNative(VType, a => VNative(VType, b => typeEqualsResult(a, b)))

  private def typeEqualsResult(a: SemValue, b: SemValue): SemValue =
    if (isConcrete(a) && isConcrete(b)) (if (structurallyEqual(a, b)) Evaluator.trueValue else Evaluator.falseValue)
    else stuck(typeEqualsFQN, a, b)

  /** A fully concrete normal form: `Type`, a ground constant, or a body-less constructor applied to concrete arguments.
    * Anything else (a metavariable, a neutral parameter, a lambda/native, a function type) is not concrete.
    */
  private def isConcrete(v: SemValue): Boolean = v match {
    case VType                        => true
    case VConst(_)                    => true
    case VTopDef(_, None, spine)      => spine.toList.forall(isConcrete)
    case _                            => false
  }

  /** Structural equality of two concrete normal forms (assumes both are [[isConcrete]]): same head and all arguments
    * equal, recursing through the spine. Distinct concrete kinds (e.g. `Type` vs a constructor) are unequal.
    */
  private def structurallyEqual(a: SemValue, b: SemValue): Boolean = (a, b) match {
    case (VType, VType)                                 => true
    case (VConst(g1), VConst(g2))                       => groundEquals(g1, g2)
    case (VTopDef(f1, None, s1), VTopDef(f2, None, s2)) =>
      val l1 = s1.toList
      val l2 = s2.toList
      f1 === f2 && l1.length === l2.length && l1.zip(l2).forall { case (x, y) => structurallyEqual(x, y) }
    case _                                              => false
  }

  /** Structural equality for ground values, mirroring `Unifier.groundEquals` — the bottom of [[structurallyEqual]] for
    * the ground constants carried by a `VConst` (an integer bound, a string, a `Bool`), including their type field.
    */
  private def groundEquals(g1: GroundValue, g2: GroundValue): Boolean = (g1, g2) match {
    case (GroundValue.Type, GroundValue.Type)                                   => true
    case (GroundValue.Direct(v1, t1), GroundValue.Direct(v2, t2))               => v1 == v2 && groundEquals(t1, t2)
    case (GroundValue.Structure(n1, a1, t1), GroundValue.Structure(n2, a2, t2)) =>
      n1 === n2 && a1.length === a2.length && groundEquals(t1, t2) &&
      a1.zip(a2).forall { case (l, r) => groundEquals(l, r) }
    case _                                                                      => false
  }
}
