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
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType.*

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
      } yield betaReduce(TypeApplication(target.as(targetValue), argument.as(argValue)))
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
    } else if (rawVfqn === typeFQN) {
      TypeReference(typeFQN).pure[CompilerIO]
    } else if (
      rawVfqn.name.name.charAt(0).isUpper || rawVfqn.name.qualifier === Qualifier.Type
    ) {
      // Uppercase or Type-qualified: could be a constructor (structural) or a type-level function.
      // Check if the value has a body and evaluates to a concrete type (no type variables).
      val typeFqn =
        if (rawVfqn.name.qualifier === Qualifier.Type) rawVfqn
        else ValueFQN(rawVfqn.moduleName, QualifiedName(rawVfqn.name.name, Qualifier.Type))
      getFact(OperatorResolvedValue.Key(rawVfqn)).flatMap {
        case Some(fact) =>
          fact.runtime match {
            case Some(body) =>
              evaluate(body, evaluating + rawVfqn, callSite = Some(sourced)).map(SymbolicType.betaReduce).flatMap {
                result =>
                  if (containsAnyTypeVariable(result)) TypeReference(typeFqn).pure[CompilerIO]
                  else result.pure[CompilerIO]
              }
            case None       =>
              rawVfqn.name.qualifier match {
                case _: Qualifier.Ability | _: Qualifier.AbilityImplementation =>
                  // Abstract associated type: treat as unification variable so it can be bound during type checking
                  TypeVariable(rawVfqn.show + "$").pure[CompilerIO]
                case _                                                         =>
                  TypeReference(typeFqn).pure[CompilerIO]
              }
          }
        case None       => TypeReference(typeFqn).pure[CompilerIO]
      }
    } else {
      // It's a type-level function, inline and beta-reduce
      getFact(OperatorResolvedValue.Key(rawVfqn)).flatMap {
        case Some(fact) =>
          fact.runtime match {
            case Some(body) =>
              evaluate(body, evaluating + rawVfqn, callSite = Some(sourced)).map(SymbolicType.betaReduce)
            case None       => TypeReference(rawVfqn).pure[CompilerIO]
          }
        case None       => compilerAbort(sourced.as("Can not evaluate referenced value."))
      }
    }

  private def containsAnyTypeVariable(st: SymbolicType): Boolean =
    st match {
      case TypeVariable(_)                => true
      case TypeApplication(target, arg)   => containsAnyTypeVariable(target.value) || containsAnyTypeVariable(arg.value)
      case TypeLambda(_, paramType, body) => containsAnyTypeVariable(paramType) || containsAnyTypeVariable(body.value)
      case _                              => false
    }

  private val bigIntTypeFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type))
  private val stringTypeFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type))
}
