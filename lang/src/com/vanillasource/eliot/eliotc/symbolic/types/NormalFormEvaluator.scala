package com.vanillasource.eliot.eliotc.symbolic.types

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType
import SymbolicType.*
import cats.data.StateT
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.TypeGraphIO

object NormalFormEvaluator {

  /** Translates an expression to its structural normal form as a SymbolicType. Converts the expression tree, inlining
    * and beta-reducing value reference bodies, but not reducing the top-level result.
    */
  def evaluate(
      expression: Sourced[OperatorResolvedExpression],
      evaluating: Set[ValueFQN] = Set.empty,
      callSite: Option[Sourced[?]] = None
  ): TypeGraphIO[SymbolicType] = expression.value match {
    case OperatorResolvedExpression.IntegerLiteral(s)                                 =>
      LiteralType(s.value, bigIntTypeFQN).pure[TypeGraphIO]
    case OperatorResolvedExpression.StringLiteral(s)                                  =>
      LiteralType(s.value, stringTypeFQN).pure[TypeGraphIO]
    case OperatorResolvedExpression.ParameterReference(s)                             =>
      TypeVariable(s.value).pure[TypeGraphIO]
    case OperatorResolvedExpression.ValueReference(s, _)                              =>
      // We handle arguments in the FunctionApplication case, so this is only if no arguments
      // or normal arguments.
      evaluateValue(s.value, expression, evaluating)
    case OperatorResolvedExpression.FunctionLiteral(paramName, None, _)               =>
      StateT.liftF(compilerAbort(paramName.as("Lambda parameter type must be explicit when expression is evaluated.")))
    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramType), body) =>
      for {
        evaluatedParamType <- evaluate(paramType.map(_.signature), evaluating, callSite)
        evaluatedBody      <- evaluate(body, evaluating, callSite)
      } yield TypeLambda(paramName.value, evaluatedParamType, body.as(evaluatedBody))
    case OperatorResolvedExpression.FunctionApplication(target, argument)             =>
      // There are several different cases we need to handle:
      // - Constructors: Stay as structural elements
      // - "Type^Default": Should be handled as Type^Type, case 1
      // - Non-constructor function applied to value: Evaluate
      // - Non-constructor function applied to symbol: Introduce new symbol with type return type of function
      // TODO: above
      for {
        targetValue <- evaluate(target, evaluating, callSite)
        argValue    <- evaluate(argument, evaluating, callSite)
      } yield TypeApplication(target.as(targetValue), argument.as(argValue))
  }

  /** Evaluates a value reference by looking up its body and fully evaluating it. Values without a runtime body (data
    * types, built-ins) return a type reference.
    */
  private def evaluateValue(
      vfqn: ValueFQN,
      sourced: Sourced[?],
      evaluating: Set[ValueFQN]
  ): TypeGraphIO[SymbolicType] =
    if (evaluating.contains(vfqn)) {
      StateT.liftF(compilerAbort(sourced.as("Recursive evaluation detected.")))
    } else if (vfqn === typeFQN) {
      TypeReference(vfqn).pure[TypeGraphIO]
    } else {
      StateT.liftF(getFact(OperatorResolvedValue.Key(vfqn))).flatMap {
        case Some(resolved) =>
          resolved.runtime match {
            case Some(body) =>
              evaluate(body, evaluating + vfqn, callSite = Some(sourced))
                .map(SymbolicType.betaReduce)
            case None       => TypeReference(vfqn).pure[TypeGraphIO] // FIXME: this is certainly wrong
          }
        case None           =>
          TypeReference(vfqn).pure[TypeGraphIO] // FIXME: this is certainly wrong
      }
    }

  private val bigIntTypeFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type))
  private val stringTypeFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type))
}
