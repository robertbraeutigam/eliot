package com.vanillasource.eliot.eliotc.implementation.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.implementation.fact.{ModuleAbilityOverlapCheck, TypeExpression}
import com.vanillasource.eliot.eliotc.implementation.util.TypeExpressionEvaluator
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
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
      patterns <- markers.traverse(loadMarkerPattern)
      resolved  = markers.zip(patterns).collect { case (vfqn, Some(p)) => (vfqn, p) }
      _        <- reportOverlaps(resolved)
    } yield ModuleAbilityOverlapCheck(key.moduleName, key.abilityName)
  }

  /** Returns `(typeVars, argTypes)` for the marker function, or `None` if its signature can't be evaluated (e.g.
    * because it transitively refers to an abstract or erroring value).
    */
  private def loadMarkerPattern(markerVfqn: ValueFQN): CompilerIO[Option[(Seq[String], Seq[TypeExpression])]] =
    getFact(OperatorResolvedValue.Key(markerVfqn)).flatMap {
      case None           => None.pure[CompilerIO]
      case Some(resolved) =>
        recover(
          TypeExpressionEvaluator
            .evaluate(resolved.typeStack.as(resolved.typeStack.value.signature))
            .map { signatureType =>
              val typeParams = TypeExpression.extractLeadingLambdaParams(signatureType).map(_._1)
              val body       = TypeExpression.stripLeadingLambdas(signatureType)
              val argTypes   = TypeExpression.extractFunctionArgTypes(body)
              Option((typeParams, argTypes))
            }
        )(None)
    }

  private def reportOverlaps(
      withPatterns: Seq[(ValueFQN, (Seq[String], Seq[TypeExpression]))]
  ): CompilerIO[Unit] =
    (for {
      i                   <- withPatterns.indices
      j                   <- (i + 1) until withPatterns.size
      (v1, (vars1, args1)) = withPatterns(i)
      (v2, (vars2, args2)) = withPatterns(j)
      if TypeExpression.patternsUnify(args1, vars1.toSet, args2, vars2.toSet)
    } yield (v1, v2)).toList.traverse_ { case (v1, v2) =>
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
}
