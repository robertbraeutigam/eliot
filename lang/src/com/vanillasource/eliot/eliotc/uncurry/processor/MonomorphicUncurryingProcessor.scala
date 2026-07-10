package com.vanillasource.eliot.eliotc.uncurry.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.monomorphize.lowering.RepresentationLowering.representationOf
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.uncurry.fact.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.*

import scala.annotation.tailrec

/** Processor that uncurries monomorphic values to a specific arity.
  *
  * Input: MonomorphicValue (monomorphized, concrete types) Output: UncurriedMonomorphicValue (multi-parameter form with
  * specified arity)
  */
class MonomorphicUncurryingProcessor
    extends TransformationProcessor[MonomorphicValue.Key, UncurriedMonomorphicValue.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: UncurriedMonomorphicValue.Key,
      monomorphicValue: MonomorphicValue
  ): CompilerIO[UncurriedMonomorphicValue] =
    for {
      _                               <- debug[CompilerIO](s"Uncurrying ${key.vfqn} (${key.typeArguments.size} type args) to ${key.arity}")
      // Run the refinement channel (shadow mode, Step 2 of docs/bounds-as-refinements.md) for this instance before
      // lowering it: producing the table triggers the shadow check (channel-computed interval == the type's interval at
      // every arithmetic/join node), which fails the build on a divergence. Every value the backend generates is
      // uncurried, so this is a natural once-per-generated-value gate on the critical path. The `intervals` payload is
      // ALSO the source of each Int node's machine representation (Step-6 staging, "Staged R2"): a position-keyed map is
      // threaded into `lowerUncurried` so `representationOf` derives an `Int`'s layout from the channel's per-node
      // interval, keeping the `Int[min, max]` type only as the shadow cross-check (see `RepresentationLowering`). In
      // shadow mode the two coincide; after the flag day the table is the only interval source.
      refinementTable                 <- getFactIfProduced(RefinementTable.Key(key.vfqn, key.typeArguments))
      // Position-keyed interval map, keeping only *unambiguous* positions. In shadow mode the checker splices a
      // `nativeWiden(x)` coercion at the *same* source position as its operand `x`, so the channel records two distinct
      // intervals there (the operand's own range and the widened range); such a position is dropped, and lowering falls
      // back to the type (the source of truth in shadow mode). These coercion wrappers exist only while `Int` carries
      // its bounds — they vanish at the flag day when `Coerce`/`nativeWiden` go — so no genuine post-flag-day node is
      // ambiguous by position.
      intervalByPosition               = refinementTable
                                           .map(unambiguousIntervalsByPosition)
                                           .getOrElse(Map.empty[PositionRange, (BigInt, BigInt)])
      (parameterTypes, returnType)    <- extractParameters(monomorphicValue.name, monomorphicValue.signature, key.arity)
      (parameterNames, convertedBody) <- monomorphicValue.runtime match {
                                           case Some(body) =>
                                             convertBody(body, key.arity)
                                           case None       =>
                                             (
                                               Range
                                                 .inclusive(1, key.arity)
                                                 .map(i => monomorphicValue.name.as("$p" + i)),
                                               None
                                             ).pure[CompilerIO]
                                         }
      // Phase 3: lower every descriptor-relevant type to its machine representation (e.g. `Int[0, 255]` -> `JvmByte`).
      // The instance identity (`typeArguments`, the lookup key, and the `MonomorphicValueReference` type arguments
      // inside the body) is left untouched — only the types that become JVM descriptors are rewritten.
      loweredSignature                 <- representationOf(monomorphicValue.signature, monomorphicValue.name)
      loweredReturnType                <- representationOf(returnType, monomorphicValue.name)
      loweredParameterTypes            <- parameterTypes.traverse(representationOf(_, monomorphicValue.name))
      loweredBody                      <- convertedBody.traverse(b =>
                                            lowerUncurried(b.value, monomorphicValue.name, intervalByPosition.get(b.range), intervalByPosition)
                                              .map(b.as)
                                          )
    } yield UncurriedMonomorphicValue(
      vfqn = key.vfqn,
      typeArguments = key.typeArguments,
      arity = key.arity,
      name = monomorphicValue.name,
      signature = loweredSignature,
      parameters = parameterNames.zip(loweredParameterTypes).map(MonomorphicParameterDefinition(_, _)),
      returnType = loweredReturnType,
      body = loweredBody.map(_.map(_.expression))
    )

  /** Collapse the channel's per-node intervals to a position-keyed map, keeping a position only when every entry at it
    * agrees on one interval. A position carrying two or more distinct intervals is a shadow-mode `nativeWiden`
    * collision (the coercion wrapper shares its operand's source position) — omitted so lowering falls back to the type.
    */
  private def unambiguousIntervalsByPosition(
      table: RefinementTable
  ): Map[PositionRange, (BigInt, BigInt)] =
    table.intervals
      .groupBy(_.position)
      .collect {
        case (position, entries) if entries.map(ni => (ni.min, ni.max)).distinct.sizeIs == 1 =>
          position -> (entries.head.min, entries.head.max)
      }

  /** Lower every nested `expressionType` (and `FunctionLiteral` parameter type) of an uncurried expression to its
    * machine representation (Phase 3). The `MonomorphicValueReference` type arguments are deliberately left untouched:
    * they are the instance-lookup key into [[UncurriedMonomorphicValue]] and (for `integerLiteral[V]`) carry the literal
    * constant, neither of which is a descriptor.
    *
    * `nodeInterval` is the refinement channel's interval for *this* node (looked up by the caller from `intervals` at the
    * node's source position); it sources the layout of an `Int`-typed node (Step-6 staging). `intervals` is the whole
    * per-node map, threaded on so child nodes — which the caller holds as `Sourced`, hence with their own positions —
    * can be looked up during the recursion.
    */
  private def lowerUncurried(
      expr: UncurriedMonomorphicExpression,
      context: Sourced[?],
      nodeInterval: Option[(BigInt, BigInt)],
      intervals: Map[PositionRange, (BigInt, BigInt)]
  ): CompilerIO[UncurriedMonomorphicExpression] =
    for {
      loweredType       <- representationOf(expr.expressionType, context, nodeInterval)
      loweredExpression <- lowerExpression(expr.expression, context, intervals)
    } yield UncurriedMonomorphicExpression(loweredType, loweredExpression)

  private def lowerExpression(
      expression: UncurriedMonomorphicExpression.Expression,
      context: Sourced[?],
      intervals: Map[PositionRange, (BigInt, BigInt)]
  ): CompilerIO[UncurriedMonomorphicExpression.Expression] =
    expression match {
      case FunctionApplication(target, arguments) =>
        for {
          loweredTarget    <- lowerUncurried(target.value, context, intervals.get(target.range), intervals).map(target.as)
          loweredArguments <- arguments.traverse(a =>
                                lowerUncurried(a.value, context, intervals.get(a.range), intervals).map(a.as)
                              )
        } yield FunctionApplication(loweredTarget, loweredArguments)
      case FunctionLiteral(parameters, body)      =>
        for {
          loweredParameters <- parameters.traverse(p =>
                                 representationOf(p.parameterType, context).map(MonomorphicParameterDefinition(p.name, _))
                               )
          loweredBody       <- lowerUncurried(body.value, context, intervals.get(body.range), intervals).map(body.as)
        } yield FunctionLiteral(loweredParameters, loweredBody)
      case other                                  =>
        // IntegerLiteral / StringLiteral / ParameterReference / MonomorphicValueReference carry no nested expression
        // types to lower (their own `expressionType` is handled by `lowerUncurried`).
        other.pure[CompilerIO]
    }

  /** Extract parameter types from a function type GroundValue up to the specified arity. */
  private def extractParameters(
      name: Sourced[?],
      signature: GroundValue,
      arity: Int
  ): CompilerIO[(Seq[GroundValue], GroundValue)] =
    if (arity === 0) {
      (Seq.empty, signature).pure[CompilerIO]
    } else {
      signature.asFunctionType match {
        case Some((paramType, returnType)) =>
          extractParameters(name, returnType, arity - 1).map { (restParams, restReturn) =>
            (paramType +: restParams, restReturn)
          }
        case None                          =>
          compilerAbort(
            name.as("Could not extract parameters."),
            Seq(s"Remaining arity: $arity", s"Signature: ${signature.show}")
          )
      }
    }

  private def convertBody(
      expression: Sourced[MonomorphicExpression.Expression],
      arity: Int
  ): CompilerIO[(Seq[Sourced[String]], Option[Sourced[UncurriedMonomorphicExpression]])] =
    for {
      (parameterNames, body) <- stripLambdas(expression.value, arity, expression)
    } yield (parameterNames, Some(expression.as(convertExpression(body))))

  /** Strip lambda expressions from the body up to a maximum count. */
  private def stripLambdas(
      expression: MonomorphicExpression.Expression,
      arity: Int,
      sourced: Sourced[?]
  ): CompilerIO[(Seq[Sourced[String]], MonomorphicExpression)] =
    if (arity === 0) {
      // Use a dummy type for the remaining expression — it's re-computed during conversion
      (Seq.empty, MonomorphicExpression(GroundValue.Type, expression)).pure[CompilerIO]
    } else {
      expression match {
        case MonomorphicExpression.FunctionLiteral(parameterName, parameterType, body) =>
          for {
            (restParameters, restExpression) <- stripLambdas(body.value.expression, arity - 1, sourced)
          } yield (parameterName +: restParameters, restExpression)
        case _                                                                          =>
          compilerAbort(sourced.as("Could not strip enough parameters from expression."))
      }
    }

  /** Convert a MonomorphicExpression to UncurriedMonomorphicExpression. */
  private def convertExpression(expr: MonomorphicExpression): UncurriedMonomorphicExpression =
    expr.expression match {
      case MonomorphicExpression.IntegerLiteral(value)                              =>
        UncurriedMonomorphicExpression(expr.expressionType, UncurriedMonomorphicExpression.IntegerLiteral(value))
      case MonomorphicExpression.StringLiteral(value)                               =>
        UncurriedMonomorphicExpression(expr.expressionType, UncurriedMonomorphicExpression.StringLiteral(value))
      case MonomorphicExpression.ParameterReference(paramName)                      =>
        UncurriedMonomorphicExpression(expr.expressionType, UncurriedMonomorphicExpression.ParameterReference(paramName))
      case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)          =>
        UncurriedMonomorphicExpression(
          expr.expressionType,
          UncurriedMonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)
        )
      case MonomorphicExpression.FunctionApplication(target, argument)              =>
        UncurriedMonomorphicExpression(expr.expressionType, convertApplication(target, argument))
      case MonomorphicExpression.FunctionLiteral(paramName, paramType, body)        =>
        UncurriedMonomorphicExpression(expr.expressionType, convertLambda(paramName, paramType, body))
    }

  /** Convert a function application, flattening nested applications. */
  private def convertApplication(
      target: Sourced[MonomorphicExpression],
      argument: Sourced[MonomorphicExpression]
  ): UncurriedMonomorphicExpression.Expression = {
    val (finalTarget, arguments) = flattenApplication(target, Seq(argument))
    val convertedTarget          = finalTarget.as(convertExpression(finalTarget.value))
    val convertedArgs            = arguments.map(se => se.as(convertExpression(se.value)))
    UncurriedMonomorphicExpression.FunctionApplication(convertedTarget, convertedArgs)
  }

  /** Flatten nested function applications into a single application with multiple arguments. */
  @tailrec
  private def flattenApplication(
      target: Sourced[MonomorphicExpression],
      arguments: Seq[Sourced[MonomorphicExpression]]
  ): (Sourced[MonomorphicExpression], Seq[Sourced[MonomorphicExpression]]) =
    target.value.expression match {
      case MonomorphicExpression.FunctionApplication(innerTarget, innerArg) =>
        flattenApplication(innerTarget, innerArg +: arguments)
      case _                                                                  =>
        (target, arguments)
    }

  /** Convert a function literal, flattening nested lambdas. */
  private def convertLambda(
      paramName: Sourced[String],
      paramType: GroundValue,
      body: Sourced[MonomorphicExpression]
  ): UncurriedMonomorphicExpression.Expression = {
    val firstParam          = MonomorphicParameterDefinition(paramName, paramType)
    val (params, finalBody) = flattenLambda(Seq(firstParam), body)
    UncurriedMonomorphicExpression.FunctionLiteral(params, body.as(convertExpression(finalBody.value)))
  }

  /** Flatten nested lambda expressions into a single lambda with multiple parameters. */
  @tailrec
  private def flattenLambda(
      parameters: Seq[MonomorphicParameterDefinition],
      body: Sourced[MonomorphicExpression]
  ): (Seq[MonomorphicParameterDefinition], Sourced[MonomorphicExpression]) =
    body.value.expression match {
      case MonomorphicExpression.FunctionLiteral(paramName, paramType, innerBody) =>
        flattenLambda(parameters :+ MonomorphicParameterDefinition(paramName, paramType), innerBody)
      case _                                                                       =>
        (parameters, body)
    }
}
