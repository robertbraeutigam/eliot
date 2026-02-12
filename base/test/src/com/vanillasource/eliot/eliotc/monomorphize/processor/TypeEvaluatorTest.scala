package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.data.Chain
import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}

class TypeEvaluatorTest extends ProcessorTest() {
  private val intVfqn    = ValueFQN(testModuleName, "Int")
  private val stringVfqn = ValueFQN(testModuleName, "String")
  private val boolVfqn   = ValueFQN(testModuleName, "Bool")
  private val intType    = Types.dataType(intVfqn)
  private val stringType = Types.dataType(stringVfqn)
  private val boolType   = Types.dataType(boolVfqn)

  private val functionDataTypeVfqn = ValueFQN(ModuleName.systemFunctionModuleName, "Function$DataType")

  private def functionType(paramType: Value, returnType: Value): Value =
    Value.Structure(
      Map(
        "$typeName" -> Value.Direct(functionDataTypeVfqn, Types.fullyQualifiedNameType),
        "A"         -> paramType,
        "B"         -> returnType
      ),
      Value.Type
    )

  /** Create a NamedEvaluable for a 0-arity data type (just returns ConcreteValue of itself). */
  private def simpleDataTypeEvaluable(vfqn: ValueFQN): NamedEvaluable =
    NamedEvaluable(vfqn, ConcreteValue(Types.dataType(vfqn)))

  /** Create the NamedEvaluable for Function$DataType (a curried NativeFunction). */
  private def functionDataTypeEvaluable: NamedEvaluable =
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

  "TypeEvaluator" should "evaluate concrete value to type ref" in {
    val expr = ConcreteValue(Types.dataType(intVfqn))
    runEvaluator(expr, Map.empty, Seq(simpleDataTypeEvaluable(intVfqn)))
      .asserting(_ shouldBe intType)
  }

  it should "substitute type parameter with concrete type" in {
    val expr = ParameterReference("A", Value.Type)
    runEvaluator(expr, Map("A" -> intType), Seq.empty)
      .asserting(_ shouldBe intType)
  }

  it should "fail on unbound type parameter" in {
    val expr = ParameterReference("A", Value.Type)
    runEvaluatorForError(expr, Map.empty, Seq.empty)
      .asserting(_ shouldBe "Type expression contains unsubstituted parameter: A")
  }

  it should "evaluate function type with substitution" in {
    val expr = ExpressionValue.functionType(
      ParameterReference("A", Value.Type),
      ParameterReference("B", Value.Type)
    )
    runEvaluator(expr, Map("A" -> intType, "B" -> stringType), Seq(functionDataTypeEvaluable))
      .asserting(_ shouldBe functionType(intType, stringType))
  }

  it should "evaluate nested function types" in {
    val expr = ExpressionValue.functionType(
      ExpressionValue.functionType(
        ParameterReference("A", Value.Type),
        ParameterReference("B", Value.Type)
      ),
      ParameterReference("A", Value.Type)
    )
    runEvaluator(expr, Map("A" -> intType, "B" -> stringType), Seq(functionDataTypeEvaluable))
      .asserting(_ shouldBe functionType(functionType(intType, stringType), intType))
  }

  it should "evaluate concrete function type without substitution" in {
    val expr = ExpressionValue.functionType(
      ConcreteValue(Types.dataType(intVfqn)),
      ConcreteValue(Types.dataType(stringVfqn))
    )
    runEvaluator(
      expr,
      Map.empty,
      Seq(functionDataTypeEvaluable, simpleDataTypeEvaluable(intVfqn), simpleDataTypeEvaluable(stringVfqn))
    ).asserting(_ shouldBe functionType(intType, stringType))
  }

  it should "evaluate type application" in {
    val listVfqn = ValueFQN(testModuleName, "List")
    val listType = Types.dataType(listVfqn)
    val expr     = FunctionApplication(
      ConcreteValue(listType),
      ParameterReference("A", Value.Type)
    )
    val expected = Value.Structure(
      listType.asInstanceOf[Value.Structure].fields + ("A" -> intType),
      Value.Type
    )
    val listEvaluable = NamedEvaluable(
      listVfqn,
      NativeFunction(
        Value.Type,
        paramA =>
          ConcreteValue(
            Value.Structure(
              Map("$typeName" -> Value.Direct(listVfqn, Types.fullyQualifiedNameType), "A" -> paramA),
              Value.Type
            )
          )
      )
    )
    runEvaluator(expr, Map("A" -> intType), Seq(listEvaluable))
      .asserting(_ shouldBe expected)
  }

  it should "evaluate type parameter applied to a type" in {
    val ioVfqn  = ValueFQN(testModuleName, "IO$DataType")
    val unitVfqn = ValueFQN(testModuleName, "Unit")
    val unitType = Types.dataType(unitVfqn)
    val ioType   = Types.dataType(ioVfqn)
    val ioEvaluable = NamedEvaluable(
      ioVfqn,
      NativeFunction(
        Value.Type,
        paramA =>
          ConcreteValue(
            Value.Structure(
              Map("$typeName" -> Value.Direct(ioVfqn, Types.fullyQualifiedNameType), "A" -> paramA),
              Value.Type
            )
          )
      )
    )
    // Expression: M(Unit) — a type parameter applied to a concrete type
    val expr = FunctionApplication(
      ParameterReference("M", Value.Type),
      ConcreteValue(unitType)
    )
    val expected = Value.Structure(
      Map("$typeName" -> Value.Direct(ioVfqn, Types.fullyQualifiedNameType), "A" -> unitType),
      Value.Type
    )
    runEvaluator(expr, Map("M" -> ioType), Seq(ioEvaluable))
      .asserting(_ shouldBe expected)
  }

  it should "evaluate nested type parameters applied to types" in {
    val ioVfqn   = ValueFQN(testModuleName, "IO$DataType")
    val unitVfqn = ValueFQN(testModuleName, "Unit")
    val unitType = Types.dataType(unitVfqn)
    val ioType   = Types.dataType(ioVfqn)
    val ioEvaluable = NamedEvaluable(
      ioVfqn,
      NativeFunction(
        Value.Type,
        paramA =>
          ConcreteValue(
            Value.Structure(
              Map("$typeName" -> Value.Direct(ioVfqn, Types.fullyQualifiedNameType), "A" -> paramA),
              Value.Type
            )
          )
      )
    )
    // Expression: Function(A, M(Unit)) — matching the HelloWorld::f pattern
    val expr = ExpressionValue.functionType(
      ParameterReference("A", Value.Type),
      FunctionApplication(
        ParameterReference("M", Value.Type),
        ConcreteValue(unitType)
      )
    )
    val expectedIoUnit = Value.Structure(
      Map("$typeName" -> Value.Direct(ioVfqn, Types.fullyQualifiedNameType), "A" -> unitType),
      Value.Type
    )
    runEvaluator(expr, Map("M" -> ioType, "A" -> stringType), Seq(ioEvaluable, functionDataTypeEvaluable))
      .asserting(_ shouldBe functionType(stringType, expectedIoUnit))
  }

  it should "fail on type-level lambda" in {
    // Use a body that doesn't require substitution so the FunctionLiteral survives to reduction
    val expr = FunctionLiteral("A", Value.Type, ConcreteValue(intType))
    runEvaluatorForError(expr, Map.empty, Seq.empty)
      .asserting(_ shouldBe "Type expression reduced to unapplied type function")
  }

  it should "evaluate deeply nested function types" in {
    val expr = ExpressionValue.functionType(
      ParameterReference("A", Value.Type),
      ExpressionValue.functionType(
        ParameterReference("B", Value.Type),
        ExpressionValue.functionType(
          ParameterReference("C", Value.Type),
          ParameterReference("A", Value.Type)
        )
      )
    )
    runEvaluator(expr, Map("A" -> intType, "B" -> stringType, "C" -> boolType), Seq(functionDataTypeEvaluable))
      .asserting(_ shouldBe functionType(intType, functionType(stringType, functionType(boolType, intType))))
  }

  "extractTypeParams" should "extract single type parameter" in {
    val sig = FunctionLiteral("A", Value.Type, ParameterReference("A", Value.Type))
    TypeEvaluator.extractTypeParams(sig) shouldBe Seq("A")
  }

  it should "extract multiple type parameters" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      FunctionLiteral(
        "B",
        Value.Type,
        ExpressionValue.functionType(ParameterReference("A", Value.Type), ParameterReference("B", Value.Type))
      )
    )
    TypeEvaluator.extractTypeParams(sig) shouldBe Seq("A", "B")
  }

  it should "return empty for non-parameterized signature" in {
    val sig = ExpressionValue.functionType(
      ConcreteValue(Types.dataType(intVfqn)),
      ConcreteValue(Types.dataType(stringVfqn))
    )
    TypeEvaluator.extractTypeParams(sig) shouldBe Seq.empty
  }

  it should "extract all FunctionLiteral params regardless of type" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      FunctionLiteral(
        "n",
        Types.dataType(intVfqn),
        ParameterReference("n", Types.dataType(intVfqn))
      )
    )
    TypeEvaluator.extractTypeParams(sig) shouldBe Seq("A", "n")
  }

  private def runEvaluator(
      expr: ExpressionValue,
      substitution: Map[String, Value],
      evaluables: Seq[NamedEvaluable]
  ): IO[Value] =
    for {
      generator <- createGenerator(evaluables)
      result    <- TypeEvaluator.evaluateWithSubstitution(expr, substitution, sourced(())).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      => throw new Exception(s"Evaluation failed: ${errors.map(_.message).toList.mkString(", ")}")
    }

  private def runEvaluatorForError(
      expr: ExpressionValue,
      substitution: Map[String, Value],
      evaluables: Seq[NamedEvaluable]
  ): IO[String] =
    for {
      generator <- createGenerator(evaluables)
      result    <- TypeEvaluator.evaluateWithSubstitution(expr, substitution, sourced(())).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((errors, _)) if errors.nonEmpty => errors.toList.head.message
      case Left(errors) if errors.nonEmpty       => errors.toList.head.message
      case _                                     => throw new Exception("Expected error but evaluation succeeded")
    }
}
