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

  val lessThanOrEqualFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("lessThanOrEqual", Qualifier.Default))

  /** The `coerce` method of the `Coerce` ability — the by-name protocol the checker resolves (in check mode) to insert
    * an implicit, possibly representation-changing widening when an inferred type is used where a different expected
    * type built from the same constructor is wanted (e.g. `Int[0,5]` where `Int[0,10]` is expected). The widening
    * insertion itself is deferred (it lands with the `Int[MIN,MAX]` frontier); this FQN is the durable design
    * commitment — see `docs/cornerstone-fidelity-plan.md` Phase 2.
    */
  val coerceFQN: ValueFQN =
    ValueFQN(
      ModuleName(defaultSystemPackage, "Coerce"),
      QualifiedName("coerce", Qualifier.Ability("Coerce"))
    )
}
