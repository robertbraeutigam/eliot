package com.vanillasource.eliot.eliotc.eval.processor

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.FactGenerator
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class DataTypeEvaluatorTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val testFile       = new File("Test.els")
  private val testModuleName = ModuleName(Seq.empty, "Test")
  private val sourceContent  = SourceContent(testFile, Sourced(testFile, PositionRange.zero, "test source"))

  "DataTypeEvaluator" should "evaluate Int$DataType (0 params) to correct Structure" in {
    val vfqn         = ValueFQN(testModuleName, "Int$DataType")
    val resolvedValue = createResolvedValue(vfqn, Seq.empty)

    runDataTypeEvaluator(vfqn, resolvedValue).asserting { result =>
      result shouldBe ConcreteValue(
        Structure(
          Map("$typeName" -> Direct(vfqn, Type)),
          Type
        )
      )
    }
  }

  it should "evaluate Function$DataType(Int)(String) to correct Structure with both type args" in {
    val functionVfqn = ValueFQN(testModuleName, "Function$DataType")
    val intVfqn      = ValueFQN(testModuleName, "Int$DataType")
    val stringVfqn   = ValueFQN(testModuleName, "String$DataType")

    val functionResolved = createResolvedValue(functionVfqn, Seq("A", "B"))
    val intResolved      = createResolvedValue(intVfqn, Seq.empty)
    val stringResolved   = createResolvedValue(stringVfqn, Seq.empty)

    val intType    = Structure(Map("$typeName" -> Direct(intVfqn, Type)), Type)
    val stringType = Structure(Map("$typeName" -> Direct(stringVfqn, Type)), Type)

    runDataTypeEvaluatorWithFacts(functionVfqn, Seq(functionResolved, intResolved, stringResolved)).flatMap {
      case nf: NativeFunction =>
        val afterFirst = nf.body(intType)
        afterFirst match {
          case nf2: NativeFunction =>
            val afterSecond = nf2.body(stringType)
            IO.pure(afterSecond)
          case other               =>
            IO.raiseError(new Exception(s"Expected NativeFunction after first application, got: $other"))
        }
      case other              =>
        IO.raiseError(new Exception(s"Expected NativeFunction, got: $other"))
    }.asserting { result =>
      result shouldBe ConcreteValue(
        Structure(
          Map(
            "$typeName" -> Direct(functionVfqn, Type),
            "A"         -> intType,
            "B"         -> stringType
          ),
          Type
        )
      )
    }
  }

  it should "return NativeFunction for partial application Function$DataType(Int)" in {
    val functionVfqn = ValueFQN(testModuleName, "Function$DataType")
    val intVfqn      = ValueFQN(testModuleName, "Int$DataType")

    val functionResolved = createResolvedValue(functionVfqn, Seq("A", "B"))
    val intResolved      = createResolvedValue(intVfqn, Seq.empty)

    val intType = Structure(Map("$typeName" -> Direct(intVfqn, Type)), Type)

    runDataTypeEvaluatorWithFacts(functionVfqn, Seq(functionResolved, intResolved)).flatMap {
      case nf: NativeFunction =>
        IO.pure(nf.body(intType))
      case other              =>
        IO.raiseError(new Exception(s"Expected NativeFunction, got: $other"))
    }.asserting { result =>
      result shouldBe a[NativeFunction]
    }
  }

  it should "evaluate single-parameter data type Box$DataType(Int) correctly" in {
    val boxVfqn = ValueFQN(testModuleName, "Box$DataType")
    val intVfqn = ValueFQN(testModuleName, "Int$DataType")

    val boxResolved = createResolvedValue(boxVfqn, Seq("A"))
    val intResolved = createResolvedValue(intVfqn, Seq.empty)

    val intType = Structure(Map("$typeName" -> Direct(intVfqn, Type)), Type)

    runDataTypeEvaluatorWithFacts(boxVfqn, Seq(boxResolved, intResolved)).flatMap {
      case nf: NativeFunction =>
        IO.pure(nf.body(intType))
      case other              =>
        IO.raiseError(new Exception(s"Expected NativeFunction, got: $other"))
    }.asserting { result =>
      result shouldBe ConcreteValue(
        Structure(
          Map(
            "$typeName" -> Direct(boxVfqn, Type),
            "A"         -> intType
          ),
          Type
        )
      )
    }
  }

  it should "not generate fact for non-DataType names" in {
    val vfqn = ValueFQN(testModuleName, "someFunction")

    runDataTypeEvaluatorExpectNone(vfqn).asserting(_ shouldBe None)
  }

  private def sourced[T](value: T): Sourced[T] = Sourced(testFile, PositionRange.zero, value)

  /** Creates a ResolvedValue with the given type parameters. Type parameters are represented as FunctionLiterals with
    * empty parameter types.
    */
  private def createResolvedValue(vfqn: ValueFQN, typeParams: Seq[String]): ResolvedValue = {
    val typeExpr = typeParams.foldRight[Expression](Expression.ValueReference(sourced(vfqn))) { (param, body) =>
      Expression.FunctionLiteral(
        sourced(param),
        sourced(TypeStack(Seq.empty)),
        sourced(TypeStack(Seq(body)))
      )
    }
    ResolvedValue(
      vfqn,
      sourced(vfqn.name),
      None,
      sourced(TypeStack(Seq(typeExpr)))
    )
  }

  private def runDataTypeEvaluator(
      vfqn: ValueFQN,
      resolvedValue: ResolvedValue
  ): IO[InitialExpressionValue] =
    runDataTypeEvaluatorWithFacts(vfqn, Seq(resolvedValue))

  private def runDataTypeEvaluatorWithFacts(
      vfqn: ValueFQN,
      resolvedValues: Seq[ResolvedValue]
  ): IO[InitialExpressionValue] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(Seq(DataTypeEvaluator())))
      _         <- generator.registerFact(sourceContent)
      _         <- resolvedValues.traverse_(generator.registerFact)
      result    <- generator.getFact(NamedEvaluable.Key(vfqn))
    } yield result match {
      case Some(namedEvaluable) => namedEvaluable.value
      case None                 => throw new Exception(s"No NamedEvaluable generated for $vfqn")
    }

  private def runDataTypeEvaluatorExpectNone(vfqn: ValueFQN): IO[Option[NamedEvaluable]] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(Seq(DataTypeEvaluator())))
      _         <- generator.registerFact(sourceContent)
      result    <- generator.getFact(NamedEvaluable.Key(vfqn))
    } yield result
}
