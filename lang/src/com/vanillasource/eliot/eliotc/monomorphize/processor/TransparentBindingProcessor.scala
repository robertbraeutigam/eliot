package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{NativeBinding, TransparentBinding}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

/** Emits [[TransparentBinding]] facts: the post-checking analogue of [[UserValueNativesProcessor]] that caches `opaque`
  * bodies so representation lowering (Phase 3) can unfold them.
  *
  * Identical to [[UserValueNativesProcessor]] except it does NOT drop the body for `opaque` definitions — it always
  * caches `runtime`. The one-hop dependency bindings are still collected from [[NativeBinding]] (the checker's
  * semantics), which is what the bodies were type-checked against; only the *self* binding gains an unfoldable opaque
  * body.
  */
class TransparentBindingProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, TransparentBinding.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  /** Thread-safe set of FQNs currently being generated. Prevents mutual recursion deadlocks in collectBindings. */
  private val generating: java.util.Set[ValueFQN] =
    java.util.Collections.newSetFromMap(new java.util.concurrent.ConcurrentHashMap())

  override protected def generateFromKeyAndFact(
      key: TransparentBinding.Key,
      fact: InputFact
  ): CompilerIO[OutputFact] = {
    generating.add(key.vfqn)
    for {
      bodyBindings <- fact.runtime match {
                        case Some(body) => collectBindings(body.value, key.vfqn)
                        case None       => Map.empty[ValueFQN, SemValue].pure[CompilerIO]
                      }
    } yield {
      generating.remove(key.vfqn)
      val semValue = VTopDef(
        key.vfqn,
        fact.runtime.map { body =>
          Lazy {
            val evaluator = new Evaluator(vfqn => bodyBindings.get(vfqn))
            evaluator.eval(Env.empty, body.value)
          }
        },
        Spine.SNil
      )
      TransparentBinding(key.vfqn, semValue)
    }
  }

  /** Collect [[NativeBinding]]s for every ValueReference transitively reachable from `ore`, skipping FQNs that are
    * currently being generated or already accumulated. Uses [[NativeBinding]] (not [[TransparentBinding]]) for the
    * dependencies because the body's references are evaluated with the same semantics they were checked against (e.g.
    * the `fold`/`lessThanOrEqual` system natives); only the top-level value's own opaque body needs unfolding here.
    */
  private def collectBindings(
      ore: OperatorResolvedExpression,
      selfFqn: ValueFQN
  ): CompilerIO[Map[ValueFQN, SemValue]] =
    OperatorResolvedExpression
      .foldValueReferences[CompilerIO, Map[ValueFQN, SemValue]](ore, Map.empty) { (acc, vfqn) =>
        if (acc.contains(vfqn.value) || vfqn.value == selfFqn || generating.contains(vfqn.value))
          acc.pure[CompilerIO]
        else
          getFact(NativeBinding.Key(vfqn.value)).map {
            case Some(binding) => acc + (vfqn.value -> binding.semValue)
            case None          => acc
          }
      }
}
