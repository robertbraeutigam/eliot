package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.data.Chain
import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Types, Value}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType.*

class SymbolicTypeEvaluatorTest extends ProcessorTest() with MonomorphizeTestFixtures {

  private def simpleDataTypeEvaluable(vfqn: ValueFQN): NamedEvaluable =
    NamedEvaluable(vfqn, ConcreteValue(Types.dataType(vfqn)))

  "SymbolicTypeEvaluator" should "evaluate concrete type reference" in {
    val st = TypeReference(intVfqn)
    runEvaluator(st, Map.empty, Seq(simpleDataTypeEvaluable(intVfqn)))
      .asserting(_ shouldBe intType)
  }

  it should "substitute type parameter with concrete type" in {
    val st = TypeVariable("A")
    runEvaluator(st, Map("A" -> intType), Seq.empty)
      .asserting(_ shouldBe intType)
  }

  it should "fail on unbound type parameter" in {
    val st = TypeVariable("A")
    runEvaluatorForError(st, Map.empty, Seq.empty)
      .asserting(_ shouldBe "Type expression contains unsubstituted parameter: A")
  }

  it should "evaluate function type with substitution" in {
    val st = SymbolicType.functionType(TypeVariable("A"), TypeVariable("B"))
    runEvaluator(st, Map("A" -> intType, "B" -> stringType), Seq(functionDataTypeEvaluable))
      .asserting(_ shouldBe functionType(intType, stringType))
  }

  it should "evaluate nested function types" in {
    val st = SymbolicType.functionType(
      SymbolicType.functionType(TypeVariable("A"), TypeVariable("B")),
      TypeVariable("A")
    )
    runEvaluator(st, Map("A" -> intType, "B" -> stringType), Seq(functionDataTypeEvaluable))
      .asserting(_ shouldBe functionType(functionType(intType, stringType), intType))
  }

  it should "evaluate concrete function type without substitution" in {
    val st = SymbolicType.functionType(TypeReference(intVfqn), TypeReference(stringVfqn))
    runEvaluator(
      st,
      Map.empty,
      Seq(functionDataTypeEvaluable, simpleDataTypeEvaluable(intVfqn), simpleDataTypeEvaluable(stringVfqn))
    ).asserting(_ shouldBe functionType(intType, stringType))
  }

  it should "evaluate type application" in {
    val listVfqn      = ValueFQN(testModuleName, QualifiedName("List", Qualifier.Default))
    val listType      = Types.dataType(listVfqn)
    val st            = TypeApplication(unsourced(TypeReference(listVfqn)), unsourced(TypeVariable("A")))
    val expected      = Value.Structure(
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
    runEvaluator(st, Map("A" -> intType), Seq(listEvaluable))
      .asserting(_ shouldBe expected)
  }

  it should "evaluate type parameter applied to a type" in {
    val ioVfqn      = ValueFQN(testModuleName, QualifiedName("IO", Qualifier.Type))
    val unitVfqn    = ValueFQN(testModuleName, QualifiedName("Unit", Qualifier.Default))
    val unitType    = Types.dataType(unitVfqn)
    val ioType      = Types.dataType(ioVfqn)
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
    val st       = TypeApplication(unsourced(TypeVariable("M")), unsourced(TypeReference(unitVfqn)))
    val expected = Value.Structure(
      Map("$typeName" -> Value.Direct(ioVfqn, Types.fullyQualifiedNameType), "A" -> unitType),
      Value.Type
    )
    runEvaluator(st, Map("M" -> ioType), Seq(ioEvaluable))
      .asserting(_ shouldBe expected)
  }

  it should "evaluate nested type parameters applied to types" in {
    val ioVfqn         = ValueFQN(testModuleName, QualifiedName("IO", Qualifier.Type))
    val unitVfqn       = ValueFQN(testModuleName, QualifiedName("Unit", Qualifier.Default))
    val unitType       = Types.dataType(unitVfqn)
    val ioType         = Types.dataType(ioVfqn)
    val ioEvaluable    = NamedEvaluable(
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
    // Expression: Function(A, M(Unit))
    val st             = SymbolicType.functionType(
      TypeVariable("A"),
      TypeApplication(unsourced(TypeVariable("M")), unsourced(TypeReference(unitVfqn)))
    )
    val expectedIoUnit = Value.Structure(
      Map("$typeName" -> Value.Direct(ioVfqn, Types.fullyQualifiedNameType), "A" -> unitType),
      Value.Type
    )
    runEvaluator(st, Map("M" -> ioType, "A" -> stringType), Seq(ioEvaluable, functionDataTypeEvaluable))
      .asserting(_ shouldBe functionType(stringType, expectedIoUnit))
  }

  it should "fail on type-level lambda" in {
    val st = TypeLambda("A", TypeReference(Types.typeFQN), unsourced(TypeReference(intVfqn)))
    runEvaluatorForError(st, Map.empty, Seq.empty)
      .asserting(_ shouldBe "Type expression reduced to unapplied type function")
  }

  it should "evaluate deeply nested function types" in {
    val st = SymbolicType.functionType(
      TypeVariable("A"),
      SymbolicType.functionType(
        TypeVariable("B"),
        SymbolicType.functionType(TypeVariable("C"), TypeVariable("A"))
      )
    )
    runEvaluator(st, Map("A" -> intType, "B" -> stringType, "C" -> boolType), Seq(functionDataTypeEvaluable))
      .asserting(_ shouldBe functionType(intType, functionType(stringType, functionType(boolType, intType))))
  }

  private def runEvaluator(
      st: SymbolicType,
      substitution: Map[String, Value],
      evaluables: Seq[NamedEvaluable]
  ): IO[Value] =
    for {
      generator <- createGenerator(evaluables)
      result    <-
        SymbolicTypeEvaluator.evaluateWithSubstitution(st, substitution, sourced(())).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      => throw new Exception(s"Evaluation failed: ${errors.map(_.message).toList.mkString(", ")}")
    }

  private def runEvaluatorForError(
      st: SymbolicType,
      substitution: Map[String, Value],
      evaluables: Seq[NamedEvaluable]
  ): IO[String] =
    for {
      generator <- createGenerator(evaluables)
      result    <-
        SymbolicTypeEvaluator.evaluateWithSubstitution(st, substitution, sourced(())).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((errors, _)) if errors.nonEmpty => errors.toList.head.message
      case Left(errors) if errors.nonEmpty       => errors.toList.head.message
      case _                                     => throw new Exception("Expected error but evaluation succeeded")
    }
}
