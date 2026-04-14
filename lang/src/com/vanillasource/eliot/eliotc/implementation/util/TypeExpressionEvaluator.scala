package com.vanillasource.eliot.eliotc.implementation.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.implementation.fact.TypeExpression
import com.vanillasource.eliot.eliotc.implementation.fact.TypeExpression.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Evaluates type expressions to TypeExpression for structural type matching. Unlike the full Evaluator, this
  * evaluator:
  *   - Preserves unbound parameters as ParameterReference nodes
  *   - Uses structural beta-reduction only (no type checking)
  *   - Reads OperatorResolvedValue directly instead of NamedEvaluable
  */
object TypeExpressionEvaluator {

  /** Evaluate a type expression to a TypeExpression suitable for structural matching. */
  def evaluate(
      expression: Sourced[OperatorResolvedExpression],
      evaluating: Set[ValueFQN] = Set.empty,
      freeVarNames: Set[String] = Set.empty
  ): CompilerIO[TypeExpression] =
    toTypeExpression(expression.value, evaluating, freeVarNames, expression)
      .map(TypeExpression.betaReduce)

  private def toTypeExpression(
      expression: OperatorResolvedExpression,
      evaluating: Set[ValueFQN],
      freeVarNames: Set[String],
      sourced: Sourced[?]
  ): CompilerIO[TypeExpression] = expression match {
    case OperatorResolvedExpression.IntegerLiteral(s)                                 =>
      ConcreteValue(GroundValue.Direct(s.value, bigIntType)).pure[CompilerIO]
    case OperatorResolvedExpression.StringLiteral(s)                                  =>
      ConcreteValue(GroundValue.Direct(s.value, stringType)).pure[CompilerIO]
    case OperatorResolvedExpression.ParameterReference(s)                             =>
      if (freeVarNames.contains(s.value))
        ParameterReference(s.value).pure[CompilerIO]
      else
        compilerAbort(s.as(s"Unknown parameter: ${s.value}"))
    case OperatorResolvedExpression.ValueReference(s, _)                              =>
      evaluateValue(s.value, sourced, evaluating, freeVarNames)
    case OperatorResolvedExpression.FunctionLiteral(paramName, None, _)               =>
      compilerAbort(paramName.as("Lambda parameter type must be explicit when expression is evaluated."))
    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramType), body) =>
      for {
        evaluatedParamType <- toTypeExpression(paramType.value.signature, evaluating, freeVarNames, paramType)
        evaluatedBody      <- toTypeExpression(body.value, evaluating, freeVarNames + paramName.value, body)
      } yield FunctionLiteral(paramName.value, GroundValue.Type, body.as(evaluatedBody))
    case OperatorResolvedExpression.FunctionApplication(target, argument)             =>
      for {
        targetValue <- toTypeExpression(target.value, evaluating, freeVarNames, target)
        argValue    <- toTypeExpression(argument.value, evaluating, freeVarNames, argument)
      } yield FunctionApplication(target.as(targetValue), argument.as(argValue))
  }

  private def evaluateValue(
      rawVfqn: ValueFQN,
      sourced: Sourced[?],
      evaluating: Set[ValueFQN],
      freeVarNames: Set[String]
  ): CompilerIO[TypeExpression] =
    if (evaluating.contains(rawVfqn)) {
      compilerAbort(sourced.as("Recursive evaluation detected."))
    } else if (rawVfqn === WellKnownTypes.typeFQN) {
      ConcreteValue(GroundValue.Type).pure[CompilerIO]
    } else {
      getFact(OperatorResolvedValue.Key(rawVfqn)).flatMap {
        case Some(fact) =>
          fact.runtime match {
            case Some(body) =>
              evaluate(body, evaluating + rawVfqn, freeVarNames)
            case None       =>
              rawVfqn.name.qualifier match {
                case _: Qualifier.Ability | _: Qualifier.AbilityImplementation =>
                  ParameterReference(rawVfqn.show + "$").pure[CompilerIO]
                case _                                                         =>
                  ConcreteValue(dataType(rawVfqn)).pure[CompilerIO]
              }
          }
        case None       =>
          if (rawVfqn.name.qualifier === Qualifier.Type)
            ConcreteValue(dataType(rawVfqn)).pure[CompilerIO]
          else compilerAbort(sourced.as("Can not evaluate referenced value."))
      }
    }

  private def dataType(vfqn: ValueFQN): GroundValue =
    GroundValue.Structure(Map("$typeName" -> GroundValue.Direct(vfqn, GroundValue.Type)), GroundValue.Type)

  private val bigIntType: GroundValue = dataType(WellKnownTypes.bigIntFQN)

  private val stringType: GroundValue = dataType(WellKnownTypes.stringFQN)
}
