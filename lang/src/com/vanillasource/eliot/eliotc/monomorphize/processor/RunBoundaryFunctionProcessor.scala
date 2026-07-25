package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.RunBoundaryFunction
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor

/** Produces a [[RunBoundaryFunction]] tag for exactly the value FQNs a platform layer registered as run boundaries
  * ([[RunBoundaryFunction.configKey]]; the jvm plugin adds `eliot.jvm::runMain`) — the effects-as-channel carrier-stack
  * recognition tag, source (ii). For any other FQN it **declines** (an explicit `abort`, the sanctioned way a processor
  * says "no fact"), so the checker's [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker.calleePinnedParams]]
  * read returns `None` for every non-boundary callee.
  *
  * This processor is registered unconditionally in `LangProcessors`, so the [[RunBoundaryFunction.Key]] fact type always
  * has a producer — even on a lang-only build, where `boundaryFqns` is empty and every FQN is declined.
  *
  * @param boundaryFqns
  *   The registered run-boundary FQNs, threaded from [[RunBoundaryFunction.configKey]] by `LangPlugin.initialize`.
  */
class RunBoundaryFunctionProcessor(boundaryFqns: Set[ValueFQN])
    extends SingleKeyTypeProcessor[RunBoundaryFunction.Key] {

  override protected def generateFact(key: RunBoundaryFunction.Key): CompilerIO[Unit] =
    if (boundaryFqns.contains(key.vfqn)) registerFactIfClear(RunBoundaryFunction(key.vfqn))
    else abort
}
