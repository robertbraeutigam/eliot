package com.vanillasource.eliot.eliotc.uncurry

import cats.implicits.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleFunction
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.*
import com.vanillasource.eliot.eliotc.typesystem.fact.*
import com.vanillasource.eliot.eliotc.uncurry.{
  UncurriedFunction,
  UncurriedTypedExpression,
  UncurriedTypedFunctionDefinition
}

/** Processor that reverses the currying transformation applied during resolution.
  *
  * This processor restores the original user-defined function signatures from before currying by fetching the original
  * AST function definition and verifying it matches the curried structure.
  *
  * Input: TypeCheckedFunction (curried, single-parameter functions) Output: UncurriedFunction (multi-parameter
  * functions with original parameter names and multi-argument applications)
  *
  * Transformations:
  *   - Function signatures: Restored from original AST with resolved types from curried form
  *   - Function applications: ((f a) b) c → f(a, b, c)
  *
  * If the curried structure doesn't match the original AST (e.g., parameter counts differ), an error is issued.
  */
class UncurryingProcessor
    extends TransformationProcessor[TypeCheckedFunction.Key, UncurriedFunction.Key](key =>
      TypeCheckedFunction.Key(key.ffqn)
    )
    with Logging {

  override def generateFromKeyAndFact(
      key: UncurriedFunction.Key,
      typeCheckedFunction: TypeCheckedFunction
  ): CompilerIO[UncurriedFunction] =
    for {
      // Determine the original arity of function
      originalArity            <- getFactOrAbort(UnifiedModuleFunction.Key(key.ffqn)).map(_.functionDefinition.args.length)
      // Restore original arity
      (args, returnType, body) <-
        restoreArity(
          typeCheckedFunction.definition.name,
          typeCheckedFunction.definition.valueType,
          typeCheckedFunction.definition.body,
          originalArity
        )
    } yield UncurriedFunction(
      typeCheckedFunction.ffqn,
      UncurriedTypedFunctionDefinition(
        name = typeCheckedFunction.definition.name,
        genericParameters = typeCheckedFunction.definition.genericParameters,
        parameters = args,
        returnType = returnType,
        body = body
      )
    )

  private def restoreArity(
      functionName: Sourced[String],
      typeReference: TypeReference,
      body: Option[Sourced[TypedExpression]],
      arity: Int
  ): CompilerIO[(Seq[ArgumentDefinition], TypeReference, Option[Sourced[UncurriedTypedExpression]])] =
    if (arity === 0) {
      (Seq.empty, typeReference, body.map(_.map(convertExpression))).pure[CompilerIO]
    } else {
      typeReference match {
        case TypeReference.DirectTypeReference(Sourced(_, _, systemFunctionType), Seq(inputType, outputType)) =>
          // Needs to be a function to deconstruct
          ???
        case _                                                                                                =>
          compilerAbort(functionName.as(s"Could not restore arity to function."))
      }
    }

  private def convertExpression(expr: TypedExpression): UncurriedTypedExpression =
    ???
}
