package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage

object WellKnownTypes {
  val fqnFQN: ValueFQN =
    ValueFQN(
      ModuleName(Seq("eliot", "lang"), "FullyQualifiedName"),
      QualifiedName("FullyQualifiedName", Qualifier.Default)
    )

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

  /** The `coerce` method of the `Coerce` ability — the by-name protocol the checker resolves (in check mode) to insert
    * an implicit, possibly representation-changing widening when an inferred type is used where a different expected
    * type built from the same constructor is wanted (e.g. `Int[0,5]` where `Int[0,10]` is expected). The widening
    * insertion itself is deferred (it lands with the `Int[MIN,MAX]` frontier); this FQN is the durable design
    * commitment — see `docs/int-min-max-plan.md` ("Check-mode `Coerce` insertion").
    */
  val coerceFQN: ValueFQN =
    ValueFQN(
      ModuleName(defaultSystemPackage, "Coerce"),
      QualifiedName("coerce", Qualifier.Ability("Coerce"))
    )
}
