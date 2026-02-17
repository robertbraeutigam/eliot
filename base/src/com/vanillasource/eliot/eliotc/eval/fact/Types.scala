package com.vanillasource.eliot.eliotc.eval.fact

import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}

object Types {
  val fqnFQN: ValueFQN  =
    ValueFQN(
      ModuleName(Seq("eliot", "lang"), "FullyQualifiedName"),
      QualifiedName("FullyQualifiedName", Qualifier.Default)
    )
  val typeFQN: ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "lang"), "Type"), QualifiedName("Type", Qualifier.Default))

  val functionDataTypeFQN = ValueFQN(ModuleName.systemFunctionModuleName, QualifiedName("Function", Qualifier.Type))

  /** The Function data type as an ExpressionValue reference for use in type expressions.
    */
  val functionDataTypeExpr: ExpressionValue = ConcreteValue(dataType(functionDataTypeFQN))

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
          fullyQualifiedNameType
        )
      ),
      Type
    )

  val bigIntType: Value = dataType(
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Default))
  )
  val stringType: Value = dataType(
    ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Default))
  )
}
