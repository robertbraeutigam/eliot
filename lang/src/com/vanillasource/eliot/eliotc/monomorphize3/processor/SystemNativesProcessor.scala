package com.vanillasource.eliot.eliotc.monomorphize3.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{functionDataTypeFQN, typeFQN, fullyQualifiedNameType}
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize3.fact.{GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Emits NativeBinding facts for built-in system values: Function (type constructor) and Type.
  *
  * Function is wired as a curried native that takes two type args (A, B) and produces VPi(A, _ => B). This is the key
  * departure from eval's SystemValueEvaluator, which produces VConst(Structure(...)) — monomorphize3 uses VPi for all
  * function types.
  */
class SystemNativesProcessor extends SingleFactProcessor[NativeBinding.Key] {

  override def generateSingleFact(key: NativeBinding.Key): CompilerIO[NativeBinding] =
    if (key.vfqn === functionDataTypeFQN) {
      createFunctionBinding().pure[CompilerIO]
    } else if (key.vfqn === typeFQN) {
      NativeBinding(typeFQN, VType).pure[CompilerIO]
    } else {
      abort
    }

  /** Function[A, B] is a curried native: first takes A (domain), then B (codomain), and produces VPi(A, _ => B). */
  private def createFunctionBinding(): NativeBinding = {
    val nativeFunction = VNative(
      VType,
      domainGround => {
        val domainSem = groundToSem(domainGround)
        VNative(
          VType,
          codomainGround => VPi(domainSem, _ => groundToSem(codomainGround))
        )
      }
    )
    NativeBinding(functionDataTypeFQN, nativeFunction)
  }

  /** Convert a GroundValue to a SemValue, handling GroundValue.Type → VType conversion. */
  private def groundToSem(ground: GroundValue): SemValue = ground match {
    case GroundValue.Type => VType
    case other            => VConst(other)
  }
}
