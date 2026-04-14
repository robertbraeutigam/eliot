package com.vanillasource.eliot.eliotc.ability.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.ModuleAbilityOverlapCheck
import com.vanillasource.eliot.eliotc.ability.util.AbilityMatcher
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Checks, for a given `(module, abilityName)` pair, that no two implementations of that ability in that module have
  * structurally overlapping patterns.
  *
  * Runs at definition time — as soon as any call site triggers ability resolution for this ability — so overlap errors
  * point at the offending implementations rather than at call sites that merely expose the ambiguity.
  *
  * Cross-module overlap is still detected at call time by the ambiguity check in `AbilityImplementationProcessor`: the
  * definition-time check here can only see impls in one module at a time, whereas impls can legitimately live in either
  * the ability's module or a type's module.
  */
class ModuleAbilityOverlapCheckProcessor
    extends TransformationProcessor[UnifiedModuleNames.Key, ModuleAbilityOverlapCheck.Key](key =>
      UnifiedModuleNames.Key(key.moduleName)
    ) {

  override protected def generateFromKeyAndFact(
      key: ModuleAbilityOverlapCheck.Key,
      names: UnifiedModuleNames
  ): CompilerIO[ModuleAbilityOverlapCheck] = {
    // Find every marker function for this ability in this module. The marker's local name equals the ability
    // name and its qualifier carries the impl's index, which uniquely identifies the implementation.
    val markers: Seq[ValueFQN] = names.names.keys.toSeq.collect {
      case qn @ QualifiedName(n, Qualifier.AbilityImplementation(an, _))
          if n == key.abilityName && an.value == key.abilityName =>
        ValueFQN(key.moduleName, qn)
    }
    for {
      signatures <- markers.traverse(loadMarkerSignature)
      resolved    = markers.zip(signatures).collect { case (vfqn, Some(sig)) => (vfqn, sig) }
      _          <- reportOverlaps(resolved)
    } yield ModuleAbilityOverlapCheck(key.moduleName, key.abilityName)
  }

  /** Returns the marker function's ORE signature, or `None` if the marker has no resolved value fact. */
  private def loadMarkerSignature(
      markerVfqn: ValueFQN
  ): CompilerIO[Option[Sourced[OperatorResolvedExpression]]] =
    getFact(OperatorResolvedValue.Key(markerVfqn)).map(
      _.map(resolved => resolved.typeStack.as(resolved.typeStack.value.signature))
    )

  private def reportOverlaps(
      withSignatures: Seq[(ValueFQN, Sourced[OperatorResolvedExpression])]
  ): CompilerIO[Unit] =
    (for {
      i <- withSignatures.indices
      j <- (i + 1) until withSignatures.size
    } yield (withSignatures(i), withSignatures(j))).toList.traverse_ {
      case ((v1, sig1), (v2, sig2)) =>
        for {
          overlaps <- recover(AbilityMatcher.patternsOverlap(sig1, sig2))(false)
          _        <- if (overlaps) reportOverlapError(v1, v2) else ().pure[CompilerIO]
        } yield ()
    }

  private def reportOverlapError(v1: ValueFQN, v2: ValueFQN): CompilerIO[Unit] =
    // Surface the error at both impls' marker source positions so the user sees both sites.
    for {
      r1 <- getFactOrAbort(OperatorResolvedValue.Key(v1))
      r2 <- getFactOrAbort(OperatorResolvedValue.Key(v2))
      _  <- compilerError(
              r1.name.map(qn =>
                s"Overlapping ability implementation: '${qn.name}' overlaps with another implementation" +
                  s" of the same ability in the same module."
              )
            )
      _  <- compilerError(
              r2.name.map(qn =>
                s"Overlapping ability implementation: '${qn.name}' overlaps with another implementation" +
                  s" of the same ability in the same module."
              )
            )
    } yield ()
}
