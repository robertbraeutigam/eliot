package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.{compilerPackage, defaultSystemPackage, effectPackage}

object WellKnownTypes {
  val typeFQN: ValueFQN =
    ValueFQN(ModuleName(compilerPackage, "Type"), QualifiedName("Type", Qualifier.Type))

  val functionDataTypeFQN: ValueFQN =
    ValueFQN(ModuleName.systemFunctionModuleName, QualifiedName("Function", Qualifier.Type))

  /** The runtime *carrier* a function value erases to: the same `eliot.lang.Function` module as
    * [[functionDataTypeFQN]] but with [[Qualifier.Default]] (the value namespace) instead of the type-constructor
    * qualifier. [[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.carrierFQN]] collapses every function
    * type to this FQN; a backend maps it to its closure representation (the JVM maps it to `java.util.function.Function`).
    */
  val functionCarrierFQN: ValueFQN =
    ValueFQN(ModuleName.systemFunctionModuleName, QualifiedName("Function", Qualifier.Default))

  private val effectModule: ModuleName = ModuleName(effectPackage, "Effect")

  /** The `Effect` ability's `flatMap` (`eliot.effect.Effect`) — the sequencing combinator of the internal effect
    * machinery, inserted by the effect auto-lift and never named by users. Defined here (rather than as
    * `EffectMachinery` privates) so the effect phase and the checker-side effect lift share one definition.
    */
  val effectFlatMapFQN: ValueFQN = ValueFQN(effectModule, QualifiedName("flatMap", Qualifier.Ability("Effect")))

  /** The `Effect` ability's `map` — the sequencing combinator used when the continuation is pure (lifting it into the
    * carrier). See [[effectFlatMapFQN]].
    */
  val effectMapFQN: ValueFQN = ValueFQN(effectModule, QualifiedName("map", Qualifier.Ability("Effect")))

  /** The `Effect` ability's `pure` — lifts a pure value into the carrier. See [[effectFlatMapFQN]]. */
  val effectPureFQN: ValueFQN = ValueFQN(effectModule, QualifiedName("pure", Qualifier.Ability("Effect")))

  /** The opaque top carrier that erased or `Type`-typed values collapse to under
    * [[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.carrierFQN]]. It is deliberately not a declared
    * stdlib type — it is the erased-representation sentinel every backend needs (the JVM maps it to `java.lang.Object`).
    */
  val anyFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Any"), QualifiedName("Any", Qualifier.Default))

  val bigIntFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type))

  val stringFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type))

  val boolFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Bool"), QualifiedName("Bool", Qualifier.Type))

  private val boolModule: ModuleName = ModuleName(defaultSystemPackage, "Bool")

  val boolTrueFQN: ValueFQN  = ValueFQN(boolModule, QualifiedName("true", Qualifier.Default))
  val boolFalseFQN: ValueFQN = ValueFQN(boolModule, QualifiedName("false", Qualifier.Default))

  private val eitherModule: ModuleName = ModuleName(defaultSystemPackage, "Either")

  /** The `Either[E, A]` type constructor — the discharge carrier of the `Throw[E]` effect (`runThrow` reflects a
    * `{Throw[E]}` computation into an `Either[E, A]`). Abstract in the base layer (`type Either[E, A]`), redefined
    * concretely per platform (the `jvm` layer for the runtime phase, the compiler platform for the compile-time phase).
    * The effectful-signatures discharge (W2) reads back a `{Throw[String]} Type`
    * signature as a ground `Either[String, Type]` and inspects its head by [[leftFQN]]/[[rightFQN]].
    */
  val eitherFQN: ValueFQN = ValueFQN(eitherModule, QualifiedName("Either", Qualifier.Type))

  /** The `Left` constructor of [[eitherFQN]] (the error case, by convention). A value constructor, so [[Qualifier.Default]]
    * (the value namespace). The discharge step recognises `Left(msg)` as a guard rejection — `compilerAbort` with `msg`.
    */
  val leftFQN: ValueFQN = ValueFQN(eitherModule, QualifiedName("Left", Qualifier.Default))

  /** The `Right` constructor of [[eitherFQN]] (the success case). A value constructor, so [[Qualifier.Default]]. The
    * discharge step reads `Right(t)` as the resolved return type `t`.
    */
  val rightFQN: ValueFQN = ValueFQN(eitherModule, QualifiedName("Right", Qualifier.Default))

  /** `integerLiteral[V]: Int` — the platform-independent literal protocol. `CoreExpressionConverter` desugars a
    * value-position integer literal `n` into `integerLiteral[n]` so that the checker assigns it plain `Int` (post
    * flag-day `Int` carries no bounds; the range lives in the refinement channel, not the type). The literal value
    * itself is a compile-time constant carried as the erased type-argument `V`, read downstream two ways:
    * `PostDrainQuoter` recognizes this FQN at the `SemExpression → MonomorphicExpression` readback and rewrites the
    * reference into a plain `IntegerLiteral(V)` node (which every backend emits via its ordinary integer-literal path —
    * so no backend needs an `integerLiteral` intrinsic); and the refinement channel reduces the def's own
    * `integerLiteral^Meta` companion (from its `{Interval(V, V)}` return brace) at `V` to *seed* the literal's value
    * range — the one place a meta originates, kept in Eliot on this protocol (`docs/generic-refinement-merges.md`).
    */
  val integerLiteralFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Runtime"), QualifiedName("integerLiteral", Qualifier.Default))

  /** The `PatternMatch` ability (`eliot.lang.PatternMatch`) drives surface `match`: `matchdesugar` lowers a `match`
    * into a call to the ability's `handleCases` eliminator, and a backend recognises each implementation by this name
    * to emit the constructor's pattern-match dispatch. An implementation method carries
    * `Qualifier.AbilityImplementation(name, index)` with `name.value == patternMatchAbilityName`; the data type it
    * matches on is recovered from the marker signature via `ImplementationMarkerUtils.firstPatternTypeConstructorName`.
    */
  val patternMatchAbilityName: String = "PatternMatch"

  /** The eliminator method of [[patternMatchAbilityName]] — `handleCases(value, cases)`. */
  val patternMatchHandleCasesName: String = "handleCases"

  /** The `TypeMatch` ability (`eliot.lang.TypeMatch`) drives surface type-pattern matching: an implementation's
    * `typeMatch` matcher dispatches a `Type` value against one type constructor. Recognised by name the same way as
    * [[patternMatchAbilityName]].
    */
  val typeMatchAbilityName: String = "TypeMatch"

  /** The matcher method of [[typeMatchAbilityName]] — `typeMatch(value, matched, notMatched)`. */
  val typeMatchMethodName: String = "typeMatch"

  /** The ability name an implementation method belongs to, if its qualifier is an ability implementation. */
  private def abilityImplementationName(vfqn: ValueFQN): Option[String] =
    vfqn.name.qualifier match {
      case Qualifier.AbilityImplementation(name, _) => Some(name)
      case _                                        => None
    }

  /** True when `vfqn` is any method of a [[patternMatchAbilityName]] implementation. */
  def isPatternMatchImplementation(vfqn: ValueFQN): Boolean =
    abilityImplementationName(vfqn).contains(patternMatchAbilityName)

  /** True when `vfqn` is the `handleCases` eliminator of a [[patternMatchAbilityName]] implementation. */
  def isPatternMatchHandleCases(vfqn: ValueFQN): Boolean =
    isPatternMatchImplementation(vfqn) && vfqn.name.name == patternMatchHandleCasesName

  /** True when `vfqn` is any method of a [[typeMatchAbilityName]] implementation. */
  def isTypeMatchImplementation(vfqn: ValueFQN): Boolean =
    abilityImplementationName(vfqn).contains(typeMatchAbilityName)

  /** True when `vfqn` is the `typeMatch` matcher of a [[typeMatchAbilityName]] implementation. */
  def isTypeMatchTypeMatch(vfqn: ValueFQN): Boolean =
    isTypeMatchImplementation(vfqn) && vfqn.name.name == typeMatchMethodName
}
