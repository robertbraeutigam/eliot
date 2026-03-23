package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Processor that monomorphizes (specializes) generic functions.
  *
  * Given a MonomorphicValue.Key(vfqn, typeArgs), it:
  *   1. Fetches the OperatorResolvedValue for vfqn
  *   2. Evaluates the type signature with concrete type args using the eval package
  *   3. Walks the runtime expression body, computing concrete types and resolving abilities
  *   4. Produces a MonomorphicValue with fully concrete types
  */
class MonomorphicTypeCheckProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, MonomorphicValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[MonomorphicValue] =
    for {
      _             <- debug[CompilerIO](
                         s"Monomorphizing ${key.vfqn.show}, type arguments: ${key.typeArguments.map(_.show).mkString(", ")}"
                       )
      typeExprValue <- Evaluator.evaluate(
                         resolvedValue.typeStack.as(resolvedValue.typeStack.value.signature)
                       )
      analysis       = TypeParameterAnalysis.fromEvaluatedType(typeExprValue)
      _             <- if (key.typeArguments.length != analysis.allTypeParams.length &&
                           key.typeArguments.length != analysis.bodyTypeParams.length)
                         compilerAbort(
                           resolvedValue.name.as(
                             s"Type argument count mismatch: expected ${analysis.bodyTypeParams.length}, got ${key.typeArguments.length}"
                           )
                         )
                       else ().pure[CompilerIO]
      typeParamSubst = analysis.buildSubstitution(
                         key.typeArguments,
                         key.typeArguments.length == analysis.allTypeParams.length
                       )
      signature     <- Evaluator.applyTypeArgsStripped(typeExprValue, analysis.allTypeParams, typeParamSubst, resolvedValue.name)
      _             <- debug[CompilerIO](s"Monomorphized ${key.vfqn.show} to: ${signature.show}")
      runtime       <- resolvedValue.runtime.traverse { body =>
                         MonomorphicExpressionTransformer
                           .transformExpression(
                             body.value,
                             Expected.Check(signature),
                             MonoEnv(typeParamSubst, Map.empty),
                             body
                           )
                           .map(body.as)
                       }
      _             <- runtime match {
                         case Some(body) => checkReturnType(body.value.expression, signature, body)
                         case None       => ().pure[CompilerIO]
                       }
    } yield MonomorphicValue(
      key.vfqn,
      key.typeArguments,
      resolvedValue.name,
      signature,
      runtime.map(_.map(_.expression))
    )

  /** Post-hoc verification that the body's innermost return type matches the signature's return type at the same depth.
    * This complements the transformer's per-node type checking by catching mismatches in the outermost lambda chain
    * that bidirectional checking alone may not flag (e.g., when the body is a value reference whose type was resolved
    * independently of the enclosing signature).
    */
  private def checkReturnType(
      bodyExpr: MonomorphicExpression.Expression,
      signature: Value,
      source: Sourced[?]
  ): CompilerIO[Unit] =
    extractMonomorphicReturnType(bodyExpr) match {
      case Some((bodyReturnType, bodySource, depth)) =>
        val signatureReturnType = extractSignatureReturnType(signature, depth)
        if (bodyReturnType != signatureReturnType)
          compilerAbort(
            bodySource.as("Return type mismatch."),
            Seq(
              s"Expected: ${signatureReturnType.show}",
              s"Actual:   ${bodyReturnType.show}"
            )
          )
        else ().pure[CompilerIO]
      case None                                      => ().pure[CompilerIO]
    }

  /** Walk the outermost chain of FunctionLiterals in the body, returning the innermost return type, its source
    * position, and the nesting depth. Returns None if the body is not a lambda (no return type to check).
    */
  private def extractMonomorphicReturnType(
      expr: MonomorphicExpression.Expression
  ): Option[(Value, Sourced[?], Int)] =
    expr match {
      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        extractMonomorphicReturnType(body.value.expression) match {
          case Some((v, s, d)) => Some((v, s, d + 1))
          case None            => Some((body.value.expressionType, body, 1))
        }
      case _                                                => None
    }

  /** Extract the return type from the signature at the same lambda nesting depth, so the two sides can be compared. */
  private def extractSignatureReturnType(value: Value, depth: Int): Value =
    if (depth <= 0) value
    else
      value.asFunctionType match {
        case Some((_, returnType)) => extractSignatureReturnType(returnType, depth - 1)
        case None                  => value
      }
}
