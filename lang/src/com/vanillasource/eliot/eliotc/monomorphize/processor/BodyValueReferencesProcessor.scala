package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.BodyValueReferences
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

/** Emits [[BodyValueReferences]]: the value references in a value's checking body, walked exactly once and memoized so
  * the checker never re-walks bodies while transitively prefetching bindings (`Checker.ensureBinding`). Reads the same
  * [[SaturatedValue]] the checker reads; a body-less value (native / type constructor / `opaque`) has an empty checking
  * body and so contributes the empty set.
  */
class BodyValueReferencesProcessor
    extends TransformationProcessor[SaturatedValue.Key, BodyValueReferences.Key](key =>
      SaturatedValue.Key(key.vfqn, key.platform)
    ) {

  override protected def generateFromKeyAndFact(
      key: BodyValueReferences.Key,
      fact: SaturatedValue
  ): CompilerIO[BodyValueReferences] =
    fact.value.checkingRuntime
      .fold(Set.empty[ValueFQN].pure[CompilerIO])(body =>
        OperatorResolvedExpression.foldValueReferences[CompilerIO, Set[ValueFQN]](body.value, Set.empty)((acc, ref) =>
          (acc + ref.value).pure[CompilerIO]
        )
      )
      .map(BodyValueReferences(key.vfqn, key.platform, _))
}
