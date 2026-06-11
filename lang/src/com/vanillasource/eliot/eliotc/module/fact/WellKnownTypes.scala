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

  val boolTrueFQN: ValueFQN   = ValueFQN(boolModule, QualifiedName("true", Qualifier.Default))
  val boolFalseFQN: ValueFQN  = ValueFQN(boolModule, QualifiedName("false", Qualifier.Default))
  val boolAndFQN: ValueFQN    = ValueFQN(boolModule, QualifiedName("&&", Qualifier.Default))
  val typeEqualsFQN: ValueFQN = ValueFQN(boolModule, QualifiedName("typeEquals", Qualifier.Default))

  val lessThanOrEqualFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("lessThanOrEqual", Qualifier.Default))

  /** The `assignableFrom` method of the `TypeRefinement` ability — invoked by the checker to decide assignability of
    * two types built from the same type constructor.
    */
  val typeRefinementAssignableFromFQN: ValueFQN =
    ValueFQN(
      ModuleName(defaultSystemPackage, "TypeRefinement"),
      QualifiedName("assignableFrom", Qualifier.Ability("TypeRefinement"))
    )
}
