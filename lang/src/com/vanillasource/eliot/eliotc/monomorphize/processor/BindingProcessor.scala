package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.NativeBinding
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.reflect.ClassTag

/** Shared base for the two processors that turn an [[OperatorResolvedValue]] into a binding (a `VTopDef` carrying a lazy
  * body thunk) for the NbE evaluator. The checker's [[NativeBinding]] and Phase 3's
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.TransparentBinding]] differ in exactly one decision — whether an
  * `opaque` definition's body is kept — which subclasses make via [[selfBody]]
  * ([[OperatorResolvedValue.checkingRuntime]] drops it; [[OperatorResolvedValue.runtime]] keeps it).
  *
  * The subtle, error-prone parts live here once: the concurrent generation guard against mutual-recursion deadlocks, and
  * the transitive dependency collection. Dependencies are always resolved via [[NativeBinding]] (the checker's
  * semantics, the bindings the bodies were type-checked against); only the top-level value's own body selection differs
  * between phases.
  */
abstract class BindingProcessor[OutputKey <: CompilerFactKey[?]](
    getInputKey: OutputKey => OperatorResolvedValue.Key
)(using ClassTag[OutputKey])
    extends TransformationProcessor[OperatorResolvedValue.Key, OutputKey](getInputKey) {

  /** The value's own body as this phase sees it: [[OperatorResolvedValue.checkingRuntime]] for the checker (opaque
    * bodies stay stuck), [[OperatorResolvedValue.runtime]] for representation lowering (opaque bodies unfold).
    */
  protected def selfBody(fact: OperatorResolvedValue): Option[Sourced[OperatorResolvedExpression]]

  /** Wrap the computed binding in the phase-specific output fact. */
  protected def buildFact(vfqn: ValueFQN, semValue: SemValue): OutputFact

  /** Thread-safe set of FQNs currently being generated. Prevents mutual recursion deadlocks in collectBindings. */
  private val generating: java.util.Set[ValueFQN] =
    java.util.Collections.newSetFromMap(new java.util.concurrent.ConcurrentHashMap())

  override protected def generateFromKeyAndFact(key: OutputKey, fact: InputFact): CompilerIO[OutputFact] = {
    val vfqn = getInputKey(key).vfqn
    val body = selfBody(fact)
    generating.add(vfqn)
    for {
      bodyBindings <- body match {
                        case Some(b) => collectBindings(b.value, vfqn)
                        case None    => Map.empty[ValueFQN, SemValue].pure[CompilerIO]
                      }
    } yield {
      generating.remove(vfqn)
      val semValue = VTopDef(
        vfqn,
        body.map { b =>
          Lazy {
            val evaluator = new Evaluator(ref => bodyBindings.get(ref))
            evaluator.eval(Env.empty, b.value)
          }
        },
        Spine.SNil
      )
      buildFact(vfqn, semValue)
    }
  }

  /** Collect [[NativeBinding]]s for every ValueReference transitively reachable from `ore`, skipping FQNs that are
    * currently being generated (`generating` set) or already accumulated in this walk — this prevents mutual-recursion
    * deadlocks and duplicate fact fetches.
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
