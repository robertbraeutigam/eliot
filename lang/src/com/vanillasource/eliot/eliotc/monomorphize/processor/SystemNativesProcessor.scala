package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{
  addFQN,
  bigIntFQN,
  boolAndFQN,
  boolFQN,
  boolFalseFQN,
  boolFoldFQN,
  boolTrueFQN,
  functionDataTypeFQN,
  lessThanOrEqualFQN,
  maxFQN,
  minFQN,
  multiplyMaxFQN,
  multiplyMinFQN,
  subtractFQN,
  typeFQN
}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Emits NativeBinding facts for built-in system values: Function (type constructor), Type, and the compile-time Bool
  * primitives (`true`/`false`/`&&`).
  *
  * Function is wired as a curried native that takes two type args (A, B) and produces VPi(A, _ => B): the Π-former is
  * the single primitive type former, so every function type is a VPi (read back to a Function structure only at quote
  * time by the Quoter).
  *
  * Bool is declared opaque in the language (`type Bool`); its compile-time representation is supplied here as
  * `VConst(Direct(Boolean, …))` so type-level predicates (e.g. a Coerce instance's bounds check) reduce during
  * checking. `&&` reduces only when its arguments are concrete, otherwise it stays stuck so the unifier falls back to
  * ordinary unification.
  */
class SystemNativesProcessor extends SingleFactProcessor[NativeBinding.Key] {

  private val boolType: SemValue = VTopDef(boolFQN, None, Spine.SNil)

  override def generateSingleFact(key: NativeBinding.Key): CompilerIO[NativeBinding] =
    if (key.vfqn === functionDataTypeFQN) {
      createFunctionBinding().pure[CompilerIO]
    } else if (key.vfqn === typeFQN) {
      NativeBinding(typeFQN, VType).pure[CompilerIO]
    } else if (key.vfqn === boolTrueFQN) {
      NativeBinding(boolTrueFQN, Evaluator.trueValue).pure[CompilerIO]
    } else if (key.vfqn === boolFalseFQN) {
      NativeBinding(boolFalseFQN, Evaluator.falseValue).pure[CompilerIO]
    } else if (key.vfqn === boolAndFQN) {
      NativeBinding(boolAndFQN, andNative).pure[CompilerIO]
    } else if (key.vfqn === boolFoldFQN) {
      NativeBinding(boolFoldFQN, boolFoldNative).pure[CompilerIO]
    } else if (key.vfqn === lessThanOrEqualFQN) {
      NativeBinding(lessThanOrEqualFQN, lessThanOrEqualNative).pure[CompilerIO]
    } else if (key.vfqn === minFQN) {
      NativeBinding(minFQN, bigIntBinaryNative(minFQN)((a, b) => a min b)).pure[CompilerIO]
    } else if (key.vfqn === maxFQN) {
      NativeBinding(maxFQN, bigIntBinaryNative(maxFQN)((a, b) => a max b)).pure[CompilerIO]
    } else if (key.vfqn === addFQN) {
      NativeBinding(addFQN, bigIntBinaryNative(addFQN)((a, b) => a + b)).pure[CompilerIO]
    } else if (key.vfqn === subtractFQN) {
      NativeBinding(subtractFQN, bigIntBinaryNative(subtractFQN)((a, b) => a - b)).pure[CompilerIO]
    } else if (key.vfqn === multiplyMinFQN) {
      NativeBinding(multiplyMinFQN, bigIntCornerNative(multiplyMinFQN)(_.min)).pure[CompilerIO]
    } else if (key.vfqn === multiplyMaxFQN) {
      NativeBinding(multiplyMaxFQN, bigIntCornerNative(multiplyMaxFQN)(_.max)).pure[CompilerIO]
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

  /** `lessThanOrEqual(a, b): Bool` — reduces to a concrete Bool when both arguments are concrete BigIntegers,
    * otherwise stays stuck (so the unifier falls back to ordinary unification).
    */
  private def lessThanOrEqualNative: SemValue = {
    val bigIntType = VTopDef(bigIntFQN, None, Spine.SNil)
    VNative(bigIntType, a => VNative(bigIntType, b => lessThanOrEqualResult(a, b)))
  }

  private def lessThanOrEqualResult(a: SemValue, b: SemValue): SemValue = (a, b) match {
    case (VConst(GroundValue.Direct(x: BigInt, _)), VConst(GroundValue.Direct(y: BigInt, _))) =>
      if (x <= y) Evaluator.trueValue else Evaluator.falseValue
    case _                                                                                    =>
      VTopDef(lessThanOrEqualFQN, None, Spine.SNil :+ a :+ b)
  }

  /** A curried `BigInteger -> BigInteger -> BigInteger` native (e.g. `min`/`max`): reduces to a concrete BigInteger
    * when both arguments are concrete, otherwise stays stuck (so the unifier falls back to ordinary unification on the
    * still-abstract bounds). Mirrors [[lessThanOrEqualNative]]'s concreteness discipline.
    */
  private def bigIntBinaryNative(fqn: com.vanillasource.eliot.eliotc.module.fact.ValueFQN)(
      op: (BigInt, BigInt) => BigInt
  ): SemValue = {
    val bigIntType = VTopDef(bigIntFQN, None, Spine.SNil)
    VNative(bigIntType, a => VNative(bigIntType, b => bigIntBinaryResult(fqn, op, a, b)))
  }

  private def bigIntBinaryResult(
      fqn: com.vanillasource.eliot.eliotc.module.fact.ValueFQN,
      op: (BigInt, BigInt) => BigInt,
      a: SemValue,
      b: SemValue
  ): SemValue = (a, b) match {
    case (VConst(GroundValue.Direct(x: BigInt, t)), VConst(GroundValue.Direct(y: BigInt, _))) =>
      VConst(GroundValue.Direct(op(x, y), t))
    case _                                                                                    =>
      VTopDef(fqn, None, Spine.SNil :+ a :+ b)
  }

  /** A curried 4-argument `BigInteger -> … -> BigInteger` native over the corner products of `Int[a,b] * Int[c,d]`
    * (`multiplyMin`/`multiplyMax`): when all four bounds are concrete it reduces to `op(Seq(a*c, a*d, b*c, b*d))`
    * (`min`/`max`), otherwise it stays stuck so the unifier falls back to ordinary unification on the still-abstract
    * bounds. Mirrors [[bigIntBinaryNative]]'s concreteness discipline.
    */
  private def bigIntCornerNative(fqn: com.vanillasource.eliot.eliotc.module.fact.ValueFQN)(
      op: Seq[BigInt] => BigInt
  ): SemValue = {
    val bigIntType = VTopDef(bigIntFQN, None, Spine.SNil)
    def collect(acc: Seq[SemValue], remaining: Int): SemValue =
      if (remaining === 0) bigIntCornerResult(fqn, op, acc)
      else VNative(bigIntType, arg => collect(acc :+ arg, remaining - 1))
    collect(Seq.empty, 4)
  }

  private def bigIntCornerResult(
      fqn: com.vanillasource.eliot.eliotc.module.fact.ValueFQN,
      op: Seq[BigInt] => BigInt,
      args: Seq[SemValue]
  ): SemValue = args match {
    case Seq(
          VConst(GroundValue.Direct(a: BigInt, t)),
          VConst(GroundValue.Direct(b: BigInt, _)),
          VConst(GroundValue.Direct(c: BigInt, _)),
          VConst(GroundValue.Direct(d: BigInt, _))
        ) =>
      VConst(GroundValue.Direct(op(Seq(a * c, a * d, b * c, b * d)), t))
    case _ =>
      VTopDef(fqn, None, args.foldLeft(Spine.SNil: Spine)(_ :+ _))
  }

  /** `&&(a, b)`: reduces to `Direct(a && b)` when both arguments are concrete Bools, otherwise stays stuck. */
  private def andNative: SemValue =
    VNative(boolType, a => VNative(boolType, b => andResult(a, b)))

  private def andResult(a: SemValue, b: SemValue): SemValue = (a, b) match {
    case (VConst(GroundValue.Direct(x: Boolean, _)), VConst(GroundValue.Direct(y: Boolean, _))) =>
      VConst(GroundValue.Direct(x && y, Evaluator.boolGroundType))
    case _                                                                                      =>
      VTopDef(boolAndFQN, None, Spine.SNil :+ a :+ b)
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
    case _                                    => VTopDef(boolFoldFQN, None, Spine.SNil :+ cond :+ whenTrue :+ whenFalse)
  }
}
