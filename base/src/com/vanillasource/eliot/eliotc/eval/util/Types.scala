package com.vanillasource.eliot.eliotc.eval.util

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure}
import com.vanillasource.eliot.eliotc.module2.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}

object Types {

  /** The type of "Type". This is infinite recursion from here on, so all logic must stop at this type.
    */
  val typeType: Value =
    Structure(
      Map(
        "$typeName" -> Direct(
          ValueFQN(ModuleName(Seq("eliot", "compile"), "Type"), "Type"),
          dataType(ValueFQN(ModuleName(Seq("eliot", "compile"), "FullyQualifiedName"), "FullyQualifiedName"))
        )
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
          dataType(ValueFQN(ModuleName(Seq("eliot", "compile"), "FullyQualifiedName"), "FullyQualifiedName"))
        )
      ),
      typeType
    )

  val bigIntType: Value = dataType(ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), "BigInteger"))
  val stringType: Value = dataType(ValueFQN(ModuleName(defaultSystemPackage, "String"), "String"))
}
