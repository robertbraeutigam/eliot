package com.vanillasource.eliot.eliotc.eval.util

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, SelfTypedStructure, Structure}
import com.vanillasource.eliot.eliotc.module2.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}

object Types {
  private val fqnFQN: ValueFQN = ValueFQN(ModuleName(Seq("eliot", "compile"), "FullyQualifiedName"), "FullyQualifiedName")

  /** The type of "Type". This is the root of the type hierarchy - both its valueType and all field types are
    * self-referential to break the infinite recursion. All logic must stop at this type.
    */
  val typeType: Value = new SelfTypedStructure(
    Map(
      "$typeName" -> Direct(
        ValueFQN(ModuleName(Seq("eliot", "compile"), "Type"), "Type"),
        typeType
      )
    )
  )

  /** The type of FullyQualifiedName.
    */
  val fullyQualifiedNameType: Value =
    Structure(
      Map(
        "$typeName" -> Direct(fqnFQN, typeType)
      ),
      typeType
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
          typeType
        )
      ),
      typeType
    )

  val bigIntType: Value = dataType(ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), "BigInteger"))
  val stringType: Value = dataType(ValueFQN(ModuleName(defaultSystemPackage, "String"), "String"))
}
