package com.vanillasource.eliot.eliotc.symbolic.types

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{typeFQN, typeFQNType}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType
import SymbolicType.*
import com.vanillasource.eliot.eliotc.eval.fact.Value.Type

object NormalFormEvaluator {

  /** Translates an expression to its structural normal form as a SymbolicType. Converts the expression tree, inlining
    * and beta-reducing value reference bodies, but not reducing the top-level result.
    */
  def evaluate(
      expression: Sourced[OperatorResolvedExpression],
      evaluating: Set[ValueFQN] = Set.empty,
      callSite: Option[Sourced[?]] = None
  ): CompilerIO[SymbolicType] = expression.value match {
    case OperatorResolvedExpression.IntegerLiteral(s)                                 =>
      LiteralType(s.value, bigIntTypeFQN).pure[CompilerIO]
    case OperatorResolvedExpression.StringLiteral(s)                                  =>
      LiteralType(s.value, stringTypeFQN).pure[CompilerIO]
    case OperatorResolvedExpression.ParameterReference(s)                             =>
      TypeVariable(s.value).pure[CompilerIO]
    case OperatorResolvedExpression.ValueReference(s, _)                              =>
      evaluateValue(s.value, expression, evaluating)
    case OperatorResolvedExpression.FunctionLiteral(paramName, None, _)               =>
      compilerAbort(paramName.as("Lambda parameter type must be explicit when expression is evaluated."))
    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramType), body) =>
      for {
        evaluatedParamType <- evaluate(paramType.map(_.signature), evaluating, callSite)
        evaluatedBody      <- evaluate(body, evaluating, callSite)
      } yield TypeLambda(paramName.value, evaluatedParamType, body.as(evaluatedBody))
    case OperatorResolvedExpression.FunctionApplication(target, argument)             =>
      for {
        targetValue <- evaluate(target, evaluating, callSite)
        argValue    <- evaluate(argument, evaluating, callSite)
      } yield TypeApplication(target.as(targetValue), argument.as(argValue))
  }

  /** Evaluates a value reference by looking up its body and fully evaluating it. Values without a runtime body (data
    * types, built-ins) return a type reference.
    */
  private def evaluateValue(
      rawVfqn: ValueFQN,
      sourced: Sourced[?],
      evaluating: Set[ValueFQN]
  ): CompilerIO[SymbolicType] =
    if (evaluating.contains(rawVfqn)) {
      // Disallow recursion
      compilerAbort(sourced.as("Recursive evaluation detected."))
    } else {
      val vfqn = if (rawVfqn === typeFQN) typeFQNType else rawVfqn
      // Not recursive, so check the value exists
      getFact(OperatorResolvedValue.Key(vfqn)).flatMap {
        case Some(fact) =>
          if (vfqn.name.name.charAt(0).isUpper || vfqn === typeFQN || vfqn.name.qualifier === Qualifier.Type) {
            // This is a value constructor or a type constructor, so leave as structure
            TypeReference(vfqn).pure[CompilerIO]
          } else {
            // It's a type-level function. Ignore, monomoprhic phase will evaluate it.
            fact.runtime match {
              case Some(body) =>
                evaluate(body, evaluating + vfqn, callSite = Some(sourced)).map(SymbolicType.betaReduce)
              case None       => compilerAbort(sourced.as("Referenced value has no body."))
            }
          }
        case None       => compilerAbort(sourced.as("Can not evaluate referenced value."))
      }
    }

  private val bigIntTypeFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type))
  private val stringTypeFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type))
}
