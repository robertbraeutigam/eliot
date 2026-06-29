package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage

object WellKnownTypes {
  val typeFQN: ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "lang"), "Type"), QualifiedName("Type", Qualifier.Type))

  val functionDataTypeFQN: ValueFQN =
    ValueFQN(ModuleName.systemFunctionModuleName, QualifiedName("Function", Qualifier.Type))

  /** The runtime *carrier* a function value erases to: the same `eliot.lang.Function` module as
    * [[functionDataTypeFQN]] but with [[Qualifier.Default]] (the value namespace) instead of the type-constructor
    * qualifier. [[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.carrierFQN]] collapses every function
    * type to this FQN; a backend maps it to its closure representation (the JVM maps it to `java.util.function.Function`).
    */
  val functionCarrierFQN: ValueFQN =
    ValueFQN(ModuleName.systemFunctionModuleName, QualifiedName("Function", Qualifier.Default))

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

  /** The `Bool` eliminator `fold(condition, whenTrue, whenFalse)` — the only way to branch on an opaque `Bool`. Backed
    * by a compile-time native (see `SystemNativesProcessor`) that selects a branch when the condition is concrete.
    */
  val boolFoldFQN: ValueFQN = ValueFQN(boolModule, QualifiedName("fold", Qualifier.Default))

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

  private val optionModule: ModuleName = ModuleName(defaultSystemPackage, "Option")

  /** The abstract `Option` type constructor — the result type of `Coerce.coerce`. The checker builds `Option[expected]`
    * with this FQN to solve the coercion instance's target bounds by unification.
    */
  val optionFQN: ValueFQN = ValueFQN(optionModule, QualifiedName("Option", Qualifier.Type))

  /** The `some` constructor of the abstract `Option`. It is an ordinary body-less def (the NbE evaluator represents an
    * applied constructor as a stuck `VTopDef`); the checker's check-mode `Coerce` insertion recognizes this FQN to
    * discriminate a coercion result — `some payload` ⟹ accept (splice `payload`), anything else ⟹ reject. It is
    * lower-case because `Option` is an abstract `type` (not `data`), so its constructors are body-less `def`s.
    *
    * Note there is no `noneFQN`: a rejected coercion is detected as "the result head is not `some`", so the compiler
    * never names the failure constructor. `none` is therefore an ordinary library constructor declared in the stdlib
    * layer (`stdlib/.../Option.els`), not here.
    */
  val someFQN: ValueFQN = ValueFQN(optionModule, QualifiedName("some", Qualifier.Default))

  /** The abstract associated type `Combined` of the type-only `Combine[A, B]` ability. The checker resolves a
    * multi-candidate covariant metavariable (a `match` result, or a `f[A](a: A, b: A): A` result type parameter) to the
    * join of its candidate types by resolving this ability by name (`resolveAbility(combinedFQN, Seq(t1, t2))`) and
    * evaluating the resolved instance's `Combined` body. The member name is `Combined` and it carries the
    * `Qualifier.Ability("Combine")` qualifier (an associated type is a body-less ability member). Lives in the
    * compiler-coordinated [[ModuleName.compilerPackage]] (`eliot.compiler`), not the `eliot.lang` prelude.
    */
  val combinedFQN: ValueFQN =
    ValueFQN(
      ModuleName(ModuleName.compilerPackage, "Combine"),
      QualifiedName("Combined", Qualifier.Ability("Combine"))
    )

  /** The `coerce` method of the `Coerce` ability — the by-name protocol the checker resolves (in check mode) to insert
    * an implicit, possibly representation-changing widening when an inferred type is used where a different expected
    * type built from the same constructor is wanted (e.g. `Int[0,5]` where `Int[0,10]` is expected). The widening
    * insertion itself is deferred (it lands with the `Int[MIN,MAX]` frontier); this FQN is the durable design
    * commitment. Lives in the compiler-coordinated [[ModuleName.compilerPackage]] (`eliot.compiler`), not the
    * `eliot.lang` prelude.
    */
  val coerceFQN: ValueFQN =
    ValueFQN(
      ModuleName(ModuleName.compilerPackage, "Coerce"),
      QualifiedName("coerce", Qualifier.Ability("Coerce"))
    )

  /** `integerLiteral[V]: IntegerLiteralType[V]` — the platform-independent literal protocol. `CoreExpressionConverter`
    * desugars a value-position integer literal `n` into `integerLiteral[n]` so that the checker assigns it the
    * platform-chosen singleton type `IntegerLiteralType[n]` (= `Int[n, n]` on every concrete layer) without the
    * compiler ever naming `Int`. The value itself is a compile-time constant carried as the erased type-argument `V`;
    * `PostDrainQuoter` recognizes this FQN at the `SemExpression → MonomorphicExpression` readback and rewrites the
    * reference into a plain `IntegerLiteral(V)` node (typed at the node's already-computed `Int[n, n]` type), which
    * every backend emits via its ordinary integer-literal path — so no backend needs an `integerLiteral` intrinsic.
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
      case Qualifier.AbilityImplementation(name, _) => Some(name.value)
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
