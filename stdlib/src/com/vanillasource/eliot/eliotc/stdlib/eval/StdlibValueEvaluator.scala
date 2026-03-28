package com.vanillasource.eliot.eliotc.stdlib.eval

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Types.bigIntType
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Evaluating stdlib functions.
  */
class StdlibValueEvaluator extends SingleFactProcessor[NamedEvaluable.Key] {
  private val stdlibFunctions: Map[ValueFQN, ExpressionValue] = Map.from(
    Seq(
      ValueFQN(ModuleName(Seq("eliot", "lang"), "BigInteger"), QualifiedName("inc", Qualifier.Default)) ->
        NativeFunction(bigIntType, {
          case Value.Direct(n: BigInt, _) => ConcreteValue(Value.Direct(n + 1, bigIntType))
          case v => ConcreteValue(v)
        }
        )
    )
  )

  override def generateSingleFact(key: NamedEvaluable.Key): CompilerIO[NamedEvaluable] =
    stdlibFunctions.get(key.vfqn).fold(abort)(expression => NamedEvaluable(key.vfqn, expression).pure[CompilerIO])
}
