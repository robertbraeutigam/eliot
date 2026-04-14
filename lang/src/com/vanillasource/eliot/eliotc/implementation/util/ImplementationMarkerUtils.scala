package com.vanillasource.eliot.eliotc.implementation.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Expression
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Utilities for introspecting ability-implementation marker functions.
  *
  * Every implementation of an ability has a synthetic "marker" function whose local name equals the ability's name and
  * whose curried signature encodes the implementation's pattern arguments. These helpers extract information from that
  * signature — primarily the first pattern argument's type-constructor name, which is how the compiler identifies which
  * data type an implementation is for.
  *
  * The helpers read the core-level `UnifiedModuleValue` rather than the resolve-phase `ResolvedValue`, because the
  * marker's qualifier references an ability that may not be imported into every scope that needs to look it up
  * (synthetic PatternMatch/TypeMatch impls). The core signature already contains enough structural information
  * (NamedValueReference with a type name) to identify the data type without going through name resolution.
  */
object ImplementationMarkerUtils {

  /** Look up the marker for the implementation containing `methodVfqn` (any method of that impl) and return the name of
    * the type constructor referenced by its first argument.
    */
  def firstPatternTypeConstructorName(
      methodVfqn: ValueFQN,
      abilityName: String
  ): CompilerIO[Option[String]] =
    methodVfqn.name.qualifier match {
      case Qualifier.AbilityImplementation(_, index) =>
        firstPatternTypeConstructorName(methodVfqn.moduleName, abilityName, index)
      case _                                         =>
        None.pure[CompilerIO]
    }

  /** Keyed by `(moduleName, abilityName, index)` directly. */
  def firstPatternTypeConstructorName(
      moduleName: ModuleName,
      abilityName: String,
      index: Int
  ): CompilerIO[Option[String]] =
    findMarkerVfqn(moduleName, abilityName, index).flatMap {
      case None             => None.pure[CompilerIO]
      case Some(markerVfqn) =>
        getFact(UnifiedModuleValue.Key(markerVfqn)).map(
          _.flatMap(umv => firstArgTypeConstructorName(umv.namedValue.typeStack.signature))
        )
    }

  /** Variant for callers that already have an impl qualifier (e.g., iterating a names table). */
  def firstPatternTypeConstructorName(
      moduleName: ModuleName,
      abilityName: String,
      implQualifier: Qualifier
  ): CompilerIO[Option[String]] =
    implQualifier match {
      case Qualifier.AbilityImplementation(_, index) =>
        firstPatternTypeConstructorName(moduleName, abilityName, index)
      case _                                         =>
        None.pure[CompilerIO]
    }

  private def findMarkerVfqn(
      moduleName: ModuleName,
      abilityName: String,
      index: Int
  ): CompilerIO[Option[ValueFQN]] =
    getFact(UnifiedModuleNames.Key(moduleName)).map {
      case None        => None
      case Some(names) =>
        names.names.keys.collectFirst {
          case qn @ QualifiedName(n, Qualifier.AbilityImplementation(an, idx))
              if n == abilityName && an.value == abilityName && idx == index =>
            ValueFQN(moduleName, qn)
        }
    }

  private def firstArgTypeConstructorName(signature: Expression): Option[String] = {
    import Expression.*
    // Strip type-level lambdas (generics on the impl) to reach the curried value-level function type.
    def stripTypeLambdas(e: Expression): Expression =
      e match {
        case FunctionLiteral(_, Some(_), body) => stripTypeLambdas(body.value.signature)
        case other                             => other
      }
    val stripped                                    = stripTypeLambdas(signature)
    // Curried Function(Arg0, Rest) is encoded as FunctionApplication(FunctionApplication(Fn, Arg0), Rest).
    stripped match {
      case FunctionApplication(Sourced(_, _, FunctionApplication(_, arg0)), _) =>
        findTypeReferenceName(arg0.value)
      case _                                                                   => None
    }
  }

  private def findTypeReferenceName(expr: Expression): Option[String] = {
    import Expression.*
    expr match {
      case NamedValueReference(qn, _, _) if qn.value.qualifier == Qualifier.Type =>
        Some(qn.value.name)
      case FunctionApplication(target, _)                                        => findTypeReferenceName(target.value)
      case _                                                                     => None
    }
  }
}
