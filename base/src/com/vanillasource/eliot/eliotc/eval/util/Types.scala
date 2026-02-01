package com.vanillasource.eliot.eliotc.eval.util

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.module2.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}

object Types {
  val fqnFQN: ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "compile"), "FullyQualifiedName"), "FullyQualifiedName")

  val typeFQN = ValueFQN(ModuleName(Seq("eliot", "compile"), "Type"), "Type")

  /** The type of FullyQualifiedName.
    */
  val fullyQualifiedNameType: Value =
    Structure(
      Map(
        "$typeName" -> Direct(fqnFQN, Type)
      ),
      Type
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
          Type
        )
      ),
      Type
    )

  val bigIntType: Value = dataType(ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), "BigInteger"))
  val stringType: Value = dataType(ValueFQN(ModuleName(defaultSystemPackage, "String"), "String"))
}
