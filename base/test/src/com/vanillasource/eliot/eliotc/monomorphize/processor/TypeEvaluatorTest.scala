package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.FactGenerator
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class TypeEvaluatorTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val testFile       = new File("Test.els")
  private val testModuleName = ModuleName(Seq.empty, "Test")
  private val sourceContent  = SourceContent(testFile, Sourced(testFile, PositionRange.zero, "test source"))

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
        "$typeName" -> Value.Direct(functionDataTypeVfqn, Value.Type),
        "A"         -> paramType,
        "B"         -> returnType
      ),
      Value.Type
    )

  "TypeEvaluator" should "evaluate concrete value to type ref" in {
    val expr = ConcreteValue(Types.dataType(intVfqn))
    runEvaluator(expr, Map.empty).asserting(_ shouldBe intType)
  }

  it should "substitute type parameter with concrete type" in {
    val expr = ParameterReference("A", Value.Type)
    runEvaluator(expr, Map("A" -> intType)).asserting(_ shouldBe intType)
  }

  it should "fail on unbound type parameter" in {
    val expr = ParameterReference("A", Value.Type)
    runEvaluatorForError(expr, Map.empty).asserting(_ shouldBe "Unbound type parameter: A")
  }

  it should "evaluate function type with substitution" in {
    val expr = ExpressionValue.functionType(
      ParameterReference("A", Value.Type),
      ParameterReference("B", Value.Type)
    )
    runEvaluator(expr, Map("A" -> intType, "B" -> stringType))
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
    runEvaluator(expr, Map("A" -> intType, "B" -> stringType))
      .asserting(_ shouldBe functionType(
        functionType(intType, stringType),
        intType
      ))
  }

  it should "evaluate concrete function type without substitution" in {
    val expr = ExpressionValue.functionType(
      ConcreteValue(Types.dataType(intVfqn)),
      ConcreteValue(Types.dataType(stringVfqn))
    )
    runEvaluator(expr, Map.empty)
      .asserting(_ shouldBe functionType(intType, stringType))
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
    runEvaluator(expr, Map("A" -> intType))
      .asserting(_ shouldBe expected)
  }

  it should "fail on type-level lambda" in {
    val expr = FunctionLiteral("A", Value.Type, ParameterReference("A", Value.Type))
    runEvaluatorForError(expr, Map.empty)
      .asserting(_ shouldBe "Type-level lambda not yet supported in monomorphization")
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
    runEvaluator(expr, Map("A" -> intType, "B" -> stringType, "C" -> boolType))
      .asserting(_ shouldBe functionType(
        intType,
        functionType(
          stringType,
          functionType(boolType, intType)
        )
      ))
  }

  "extractUniversalParams" should "extract single universal parameter" in {
    val sig = FunctionLiteral("A", Value.Type, ParameterReference("A", Value.Type))
    TypeEvaluator.extractUniversalParams(sig) shouldBe Seq("A")
  }

  it should "extract multiple universal parameters" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      FunctionLiteral(
        "B",
        Value.Type,
        ExpressionValue.functionType(ParameterReference("A", Value.Type), ParameterReference("B", Value.Type))
      )
    )
    TypeEvaluator.extractUniversalParams(sig) shouldBe Seq("A", "B")
  }

  it should "return empty for non-universal signature" in {
    val sig = ExpressionValue.functionType(
      ConcreteValue(Types.dataType(intVfqn)),
      ConcreteValue(Types.dataType(stringVfqn))
    )
    TypeEvaluator.extractUniversalParams(sig) shouldBe Seq.empty
  }

  it should "stop at non-Type parameter type" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      FunctionLiteral(
        "x",
        Types.dataType(intVfqn),
        ParameterReference("x", Types.dataType(intVfqn))
      )
    )
    TypeEvaluator.extractUniversalParams(sig) shouldBe Seq("A")
  }

  "stripUniversalIntros" should "strip single universal introduction" in {
    val inner = ExpressionValue.functionType(ParameterReference("A", Value.Type), ParameterReference("A", Value.Type))
    val sig   = FunctionLiteral("A", Value.Type, inner)
    TypeEvaluator.stripUniversalIntros(sig) shouldBe inner
  }

  it should "strip multiple universal introductions" in {
    val inner = ExpressionValue.functionType(ParameterReference("A", Value.Type), ParameterReference("B", Value.Type))
    val sig   = FunctionLiteral("A", Value.Type, FunctionLiteral("B", Value.Type, inner))
    TypeEvaluator.stripUniversalIntros(sig) shouldBe inner
  }

  it should "not strip non-universal function literal" in {
    val sig = FunctionLiteral("x", Types.dataType(intVfqn), ParameterReference("x", Types.dataType(intVfqn)))
    TypeEvaluator.stripUniversalIntros(sig) shouldBe sig
  }

  "buildSubstitution" should "build empty map for empty inputs" in {
    TypeEvaluator.buildSubstitution(Seq.empty, Seq.empty) shouldBe Map.empty
  }

  it should "build single-entry map" in {
    TypeEvaluator.buildSubstitution(Seq("A"), Seq(intType)) shouldBe Map("A" -> intType)
  }

  it should "build multi-entry map preserving order" in {
    TypeEvaluator.buildSubstitution(Seq("A", "B", "C"), Seq(intType, stringType, boolType)) shouldBe
      Map("A" -> intType, "B" -> stringType, "C" -> boolType)
  }

  private def sourced[T](value: T): Sourced[T] = Sourced(testFile, PositionRange.zero, value)

  private def runEvaluator(
      expr: ExpressionValue,
      substitution: Map[String, Value]
  ): IO[Value] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(Seq.empty))
      _         <- generator.registerFact(sourceContent)
      result    <- TypeEvaluator.evaluate(expr, substitution, sourced(())).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      => throw new Exception(s"Evaluation failed: ${errors.map(_.message).toList.mkString(", ")}")
    }

  private def runEvaluatorForError(
      expr: ExpressionValue,
      substitution: Map[String, Value]
  ): IO[String] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(Seq.empty))
      _         <- generator.registerFact(sourceContent)
      result    <- TypeEvaluator.evaluate(expr, substitution, sourced(())).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((errors, _)) if errors.nonEmpty => errors.toList.head.message
      case Left(errors) if errors.nonEmpty       => errors.toList.head.message
      case _                                     => throw new Exception("Expected error but evaluation succeeded")
    }
}
