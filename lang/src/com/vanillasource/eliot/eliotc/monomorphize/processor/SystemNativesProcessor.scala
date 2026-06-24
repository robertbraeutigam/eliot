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
import com.vanillasource.eliot.eliotc.compiler.cache.UpToDate
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Emits NativeBinding facts for the language-intrinsic system values the compiler itself reasons about: Function (type
  * constructor), Type, and the compile-time Bool primitives `true`/`false`/`fold`.
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
  */
class SystemNativesProcessor extends SingleFactProcessor[NativeBinding.Key] {

  private val boolType: SemValue = VTopDef(boolFQN, None, Spine.SNil)

  /** The canonical stuck form of a native: a [[VStuckNative]] carrying the native's own FQN and the (not-yet-concrete)
    * arguments as its spine. Keeping the FQN is what lets distinct stuck natives stay definitionally distinct and lets
    * [[Evaluator.renormalize]] re-fire them once the arguments become concrete; the dedicated [[VStuckNative]] head
    * (rather than a body-less [[VTopDef]]) keeps the non-injective native from being injectivity-decomposed by the
    * unifier or read back as a ground type by the quoter.
    */
  private def stuck(fqn: ValueFQN, args: SemValue*): SemValue =
    VStuckNative(fqn, args.foldLeft(Spine.SNil: Spine)(_ :+ _))

  override def generateSingleFact(key: NativeBinding.Key): CompilerIO[NativeBinding] =
    // These bindings are input-less compiler constants. Depending on the always-clean `UpToDate` leaf (the value is
    // immaterial — only the edge matters) lets the incremental cache prove them unchanged on a no-change run instead of
    // treating them as source leaves and regenerating them every time. Deliberately not `getFactOrAbort`: a bundle
    // without `UpToDateProcessor` (e.g. a minimal test) just loses incrementality here, never fails.
    getFact(UpToDate.Key()) >> produceNativeBinding(key)

  private def produceNativeBinding(key: NativeBinding.Key): CompilerIO[NativeBinding] =
    if (key.vfqn === functionDataTypeFQN) {
      createFunctionBinding().pure[CompilerIO]
    } else if (key.vfqn === typeFQN) {
      NativeBinding(typeFQN, VType).pure[CompilerIO]
    } else if (key.vfqn === boolTrueFQN) {
      NativeBinding(boolTrueFQN, Evaluator.trueValue).pure[CompilerIO]
    } else if (key.vfqn === boolFalseFQN) {
      NativeBinding(boolFalseFQN, Evaluator.falseValue).pure[CompilerIO]
    } else if (key.vfqn === boolFoldFQN) {
      NativeBinding(boolFoldFQN, boolFoldNative).pure[CompilerIO]
    } else {
      abort
    }

  /** Function[A, B] is a curried native: first takes A (domain), then B (codomain), and produces VPi(A, _ => B). */
  private def createFunctionBinding(): NativeBinding = {
    val nativeFunction = VNative(
      VType,
      domain => VNative(VType, codomain => VPi(domain, _ => codomain))
    )
    NativeBinding(functionDataTypeFQN, nativeFunction)
  }

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
