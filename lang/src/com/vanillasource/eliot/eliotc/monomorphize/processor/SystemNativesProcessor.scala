package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{
  bigIntFQN,
  boolAndFQN,
  boolFQN,
  boolFalseFQN,
  boolTrueFQN,
  functionDataTypeFQN,
  lessThanOrEqualFQN,
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
  * Function is wired as a curried native that takes two type args (A, B) and produces VPi(A, _ => B). This is the key
  * departure from eval's SystemValueEvaluator, which produces VConst(Structure(...)) — monomorphize uses VPi for all
  * function types.
  *
  * Bool is declared opaque in the language (`type Bool`); its compile-time representation is supplied here as
  * `VConst(Direct(Boolean, …))` so type-level predicates (e.g. TypeRefinement) reduce during checking. `&&` reduces
  * only when its arguments are concrete, otherwise it stays stuck so the unifier falls back to ordinary unification.
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
    } else if (key.vfqn === lessThanOrEqualFQN) {
      NativeBinding(lessThanOrEqualFQN, lessThanOrEqualNative).pure[CompilerIO]
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

  /** `&&(a, b)`: reduces to `Direct(a && b)` when both arguments are concrete Bools, otherwise stays stuck. */
  private def andNative: SemValue =
    VNative(boolType, a => VNative(boolType, b => andResult(a, b)))

  private def andResult(a: SemValue, b: SemValue): SemValue = (a, b) match {
    case (VConst(GroundValue.Direct(x: Boolean, _)), VConst(GroundValue.Direct(y: Boolean, _))) =>
      VConst(GroundValue.Direct(x && y, Evaluator.boolGroundType))
    case _                                                                                      =>
      VTopDef(boolAndFQN, None, Spine.SNil :+ a :+ b)
  }
}
