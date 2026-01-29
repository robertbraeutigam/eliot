package com.vanillasource.eliot.eliotc.eval.util

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, SelfTypedStructure, Structure}
import com.vanillasource.eliot.eliotc.module2.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}

object Types {
  private val fqnFQN: ValueFQN = ValueFQN(ModuleName(Seq("eliot", "compile"), "FullyQualifiedName"), "FullyQualifiedName")

  /** The type of FullyQualifiedName. This is a SelfTypedStructure to avoid initialization cycle issues.
    */
  val fullyQualifiedNameType: Value = new SelfTypedStructure(
    Map(
      "$typeName" -> Direct(fqnFQN, fullyQualifiedNameType)
    )
  )

  /** The type of "Type". This is infinite recursion from here on, so all logic must stop at this type. Uses
    * SelfTypedStructure to allow self-referential valueType.
    */
  val typeType: Value = new SelfTypedStructure(
    Map(
      "$typeName" -> Direct(
        ValueFQN(ModuleName(Seq("eliot", "compile"), "Type"), "Type"),
        fullyQualifiedNameType
      )
    )
  )

  /** Create the type of some simple data that has no generic type parameters. For example a `data Person(name: String)`
    * would have a type of: Person$Type, without any fields, since there are no type parameters.
    * @param dataFQN
    *   The fqn of the data type. Not of the constructor, the data type!
    */
  def dataType(dataFQN: ValueFQN): Value =
    Structure(
      Map(
        "$typeName" -> Direct(
          dataFQN,
          fullyQualifiedNameType
        )
      ),
      typeType
    )

  val bigIntType: Value = dataType(ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), "BigInteger"))
  val stringType: Value = dataType(ValueFQN(ModuleName(defaultSystemPackage, "String"), "String"))
}
