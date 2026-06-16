package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage

object WellKnownTypes {
  val typeFQN: ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "lang"), "Type"), QualifiedName("Type", Qualifier.Type))

  val functionDataTypeFQN: ValueFQN =
    ValueFQN(ModuleName.systemFunctionModuleName, QualifiedName("Function", Qualifier.Type))

  val bigIntFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type))

  val stringFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type))

  val boolFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Bool"), QualifiedName("Bool", Qualifier.Type))

  private val boolModule: ModuleName = ModuleName(defaultSystemPackage, "Bool")

  val boolTrueFQN: ValueFQN  = ValueFQN(boolModule, QualifiedName("true", Qualifier.Default))
  val boolFalseFQN: ValueFQN = ValueFQN(boolModule, QualifiedName("false", Qualifier.Default))
  val boolAndFQN: ValueFQN   = ValueFQN(boolModule, QualifiedName("&&", Qualifier.Default))

  /** The `Bool` eliminator `fold(condition, whenTrue, whenFalse)` — the only way to branch on an opaque `Bool`. Backed
    * by a compile-time native (see `SystemNativesProcessor`) that selects a branch when the condition is concrete.
    */
  val boolFoldFQN: ValueFQN = ValueFQN(boolModule, QualifiedName("fold", Qualifier.Default))

  private val optionModule: ModuleName = ModuleName(defaultSystemPackage, "Option")

  /** The abstract `Option` type constructor — the result type of `Coerce.coerce`. The checker builds `Option[expected]`
    * with this FQN to solve the coercion instance's target bounds by unification.
    */
  val optionFQN: ValueFQN = ValueFQN(optionModule, QualifiedName("Option", Qualifier.Type))

  /** The `some`/`none` constructors of the abstract `Option`. They are ordinary body-less defs (the NbE evaluator
    * represents an applied constructor as a stuck `VTopDef`); the checker's check-mode `Coerce` insertion recognizes
    * these FQNs to discriminate a coercion result — `some payload` ⟹ accept (splice `payload`), `none` ⟹ reject. They
    * are lower-case because `Option` is an abstract `type` (not `data`), so its constructors are body-less `def`s.
    */
  val someFQN: ValueFQN = ValueFQN(optionModule, QualifiedName("some", Qualifier.Default))
  val noneFQN: ValueFQN = ValueFQN(optionModule, QualifiedName("none", Qualifier.Default))

  val lessThanOrEqualFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("lessThanOrEqual", Qualifier.Default))

  /** `min(a, b)` / `max(a, b)` on `BigInteger` — compile-time natives (see `SystemNativesProcessor`) that reduce when
    * both arguments are concrete, otherwise stay stuck. Used by the `Combine[Int, Int]` instance's associated
    * `Combined` type to compute the joined bound range `Int[min(aMin,bMin), max(aMax,bMax)]`.
    */
  val minFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("min", Qualifier.Default))

  val maxFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("max", Qualifier.Default))

  /** `add(a, b)` on `BigInteger` — a compile-time native (see `SystemNativesProcessor`) that reduces when both
    * arguments are concrete, otherwise stays stuck. It computes the dependent result bounds of integer addition: the
    * `+` operator on `Int[LMin, LMax]` / `Int[RMin, RMax]` has result type `Int[add(LMin, RMin), add(LMax, RMax)]`, so
    * the bounds are summed at the type level by this native while `+` itself is realized at runtime by the JVM backend
    * (`LADD`).
    */
  val addFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("add", Qualifier.Default))

  /** `subtract(a, b)` / `multiplyMin(a, b, c, d)` / `multiplyMax(a, b, c, d)` on `BigInteger` — compile-time natives
    * (see `SystemNativesProcessor`) backing the dependent result bounds of `Int`'s `-` and `*`. `subtract` gives the
    * `-` bounds `Int[subtract(LMin,RMax), subtract(LMax,RMin)]`; `multiplyMin`/`multiplyMax` give the `*` bounds as the
    * min/max of the four corner products `{a*c, a*d, b*c, b*d}` (a single corner is wrong when a range straddles zero).
    */
  val subtractFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("subtract", Qualifier.Default))

  val multiplyMinFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("multiplyMin", Qualifier.Default))

  val multiplyMaxFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("multiplyMax", Qualifier.Default))

  /** The abstract associated type `Combined` of the type-only `Combine[A, B]` ability. The checker resolves a
    * multi-candidate covariant metavariable (a `match` result, or a `f[A](a: A, b: A): A` result type parameter) to the
    * join of its candidate types by resolving this ability by name (`resolveAbility(combinedFQN, Seq(t1, t2))`) and
    * evaluating the resolved instance's `Combined` body. The member name is `Combined` and it carries the
    * `Qualifier.Ability("Combine")` qualifier (an associated type is a body-less ability member).
    */
  val combinedFQN: ValueFQN =
    ValueFQN(
      ModuleName(defaultSystemPackage, "Combine"),
      QualifiedName("Combined", Qualifier.Ability("Combine"))
    )

  /** The `coerce` method of the `Coerce` ability — the by-name protocol the checker resolves (in check mode) to insert
    * an implicit, possibly representation-changing widening when an inferred type is used where a different expected
    * type built from the same constructor is wanted (e.g. `Int[0,5]` where `Int[0,10]` is expected). The widening
    * insertion itself is deferred (it lands with the `Int[MIN,MAX]` frontier); this FQN is the durable design
    * commitment.
    */
  val coerceFQN: ValueFQN =
    ValueFQN(
      ModuleName(defaultSystemPackage, "Coerce"),
      QualifiedName("coerce", Qualifier.Ability("Coerce"))
    )
}
