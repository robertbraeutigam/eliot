package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Types, Value}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}

/** Shared test fixtures for monomorphize package tests. */
trait MonomorphizeTestFixtures { self: ProcessorTest =>
  protected val intVfqn    = ValueFQN(testModuleName, QualifiedName("Int", Qualifier.Default))
  protected val stringVfqn = ValueFQN(testModuleName, QualifiedName("String", Qualifier.Default))
  protected val boolVfqn   = ValueFQN(testModuleName, QualifiedName("Bool", Qualifier.Default))
  protected val intType    = Types.dataType(intVfqn)
  protected val stringType = Types.dataType(stringVfqn)
  protected val boolType   = Types.dataType(boolVfqn)

  protected val functionDataTypeVfqn =
    ValueFQN(ModuleName.systemFunctionModuleName, QualifiedName("Function", Qualifier.Type))

  protected def functionType(paramType: Value, returnType: Value): Value =
    Value.Structure(
      Map(
        "$typeName" -> Value.Direct(functionDataTypeVfqn, Types.fullyQualifiedNameType),
        "A"         -> paramType,
        "B"         -> returnType
      ),
      Value.Type
    )

  protected def functionDataTypeEvaluable: NamedEvaluable =
    NamedEvaluable(
      functionDataTypeVfqn,
      NativeFunction(
        Value.Type,
        paramA =>
          NativeFunction(
            Value.Type,
            paramB =>
              ConcreteValue(
                Value.Structure(
                  Map(
                    "$typeName" -> Value.Direct(functionDataTypeVfqn, Types.fullyQualifiedNameType),
                    "A"         -> paramA,
                    "B"         -> paramB
                  ),
                  Value.Type
                )
              )
          )
      )
    )
}
