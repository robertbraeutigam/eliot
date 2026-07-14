package com.vanillasource.eliot.eliotc.ability.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Expression
import com.vanillasource.eliot.eliotc.module.fact.Qualifier
import com.vanillasource.eliot.eliotc.module.fact.{ModuleAbilities, ModuleName, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.platform.Platform
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

  /** Whether `methodVfqn` denotes method `methodName` of an `implement <abilityName>[<targetTypeConstructor>, …]` block
    * — i.e. the impl-method "namespace" a native binding attaches to. This is the index-free way to recognise an
    * ability-implementation method: the impl's per-module `index` is assigned during resolution and is not knowable
    * statically, so a native supplier (the compile-time `*NativesProcessor`, the jvm backend) cannot key a static map
    * on the full FQN. It instead recognises the method by `(abilityName, methodName, first-pattern type)`: the
    * qualifier already carries the ability name and the local method name, and [[firstPatternTypeConstructorName]]
    * reads the impl's marker to confirm the instance's first type argument (the dispatch type). Reusable for any
    * "native provided directly to an ability implementation" wiring.
    */
  def isImplementationMethodFor(
      methodVfqn: ValueFQN,
      abilityName: String,
      methodName: String,
      targetTypeConstructor: String,
      platform: Platform = Platform.Runtime
  ): CompilerIO[Boolean] =
    methodVfqn.name.qualifier match {
      case Qualifier.AbilityImplementation(name, _)
          if name === abilityName && methodVfqn.name.name === methodName =>
        firstPatternTypeConstructorName(methodVfqn, abilityName, platform).map(_.contains(targetTypeConstructor))
      case _ =>
        false.pure[CompilerIO]
    }

  /** Look up the marker for the implementation containing `methodVfqn` (any method of that impl) and return the name of
    * the type constructor referenced by its first argument.
    */
  def firstPatternTypeConstructorName(
      methodVfqn: ValueFQN,
      abilityName: String,
      platform: Platform = Platform.Runtime
  ): CompilerIO[Option[String]] =
    methodVfqn.name.qualifier match {
      case Qualifier.AbilityImplementation(_, pattern) =>
        firstPatternTypeConstructorName(methodVfqn.moduleName, abilityName, pattern, platform)
      case _                                         =>
        None.pure[CompilerIO]
    }

  /** Keyed by `(moduleName, abilityName, index)` directly. */
  def firstPatternTypeConstructorName(
      moduleName: ModuleName,
      abilityName: String,
      pattern: String,
      platform: Platform
  ): CompilerIO[Option[String]] =
    findMarkerVfqn(moduleName, abilityName, pattern, platform).flatMap {
      case None             => None.pure[CompilerIO]
      case Some(markerVfqn) =>
        getFactIfProduced(UnifiedModuleValue.Key(markerVfqn, platform)).map(
          _.flatMap(umv => firstArgTypeConstructorName(umv.namedValue.typeStack.signature))
        )
    }

  /** Variant for callers that already have an impl qualifier (e.g., iterating a names table). */
  def firstPatternTypeConstructorName(
      moduleName: ModuleName,
      abilityName: String,
      implQualifier: Qualifier,
      platform: Platform
  ): CompilerIO[Option[String]] =
    implQualifier match {
      case Qualifier.AbilityImplementation(_, pattern) =>
        firstPatternTypeConstructorName(moduleName, abilityName, pattern, platform)
      case _                                         =>
        None.pure[CompilerIO]
    }

  private def findMarkerVfqn(
      moduleName: ModuleName,
      abilityName: String,
      pattern: String,
      platform: Platform
  ): CompilerIO[Option[ValueFQN]] =
    getFactIfProduced(ModuleAbilities.Key(moduleName, platform)).map(_.flatMap(_.markerOf(abilityName, pattern)))

  private def firstArgTypeConstructorName(signature: Expression): Option[String] = {
    import Expression.*
    // Strip type-level lambdas (generics on the impl) to reach the curried value-level function type.
    def stripTypeLambdas(e: Expression): Expression =
      e match {
        case FunctionLiteral(_, Some(_), body) => stripTypeLambdas(body.value)
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
