package com.vanillasource.eliot.eliotc.stdlib.plugin

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Emits NativeBinding facts for built-in stdlib functions whose bodies cannot be expressed in the language itself but
  * are needed for type-level computation (for example `inc` on [[BigInteger]] values used in dependent type
  * signatures).
  *
  * The JVM backend may provide the runtime implementation for these functions separately; this processor only seeds
  * the NbE evaluator with a native reduction rule so that calls like `I.inc` with `I = 2` reduce to `3` during type
  * checking.
  */
class StdlibNativesProcessor extends SingleFactProcessor[NativeBinding.Key] {

  private val bigIntegerModule: ModuleName = ModuleName(ModuleName.defaultSystemPackage, "BigInteger")

  private val incFQN: ValueFQN =
    ValueFQN(bigIntegerModule, QualifiedName("inc", Qualifier.Default))

  private val bindings: Map[ValueFQN, SemValue] = Map(
    incFQN -> incNative
  )

  override def generateSingleFact(key: NativeBinding.Key): CompilerIO[NativeBinding] =
    bindings.get(key.vfqn) match {
      case Some(sem) => NativeBinding(key.vfqn, sem).pure[CompilerIO]
      case None      => abort
    }

  /** `inc(n: BigInteger): BigInteger` — reduces `inc(VConst(Direct(n, _)))` to `VConst(Direct(n+1, _))`. Any other
    * argument leaves the application stuck (so it acts like an ordinary unfolded top-level reference).
    */
  private def incNative: SemValue =
    VNative(
      VTopDef(com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.bigIntFQN, None, Spine.SNil),
      {
        case VConst(GroundValue.Direct(n: BigInt, tpe)) =>
          VConst(GroundValue.Direct(n + 1, tpe))
        case other                                      =>
          // Leave application stuck: mirror the VTopDef-with-no-body behaviour.
          VTopDef(incFQN, None, Spine.SNil :+ other)
      }
    )
}
