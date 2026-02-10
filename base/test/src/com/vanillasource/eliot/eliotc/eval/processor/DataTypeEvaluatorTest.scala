package com.vanillasource.eliot.eliotc.eval.processor

import cats.data.NonEmptySeq
import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.eval.fact.Types.fullyQualifiedNameType
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedValue}

class DataTypeEvaluatorTest extends ProcessorTest(DataTypeEvaluator()) {
  private val typeVfqn      = ValueFQN(ModuleName(Seq("eliot", "compile"), "Type"), "Type")
  private val typeEvaluable = NamedEvaluable(typeVfqn, ConcreteValue(Type))

  "DataTypeEvaluator" should "evaluate Int$DataType (0 params) to correct Structure" in {
    val vfqn          = ValueFQN(testModuleName, "Int$DataType")
    val resolvedValue = createResolvedValue(vfqn, Seq.empty)

    runDataTypeEvaluator(vfqn, resolvedValue).asserting { result =>
      result shouldBe ConcreteValue(
        Structure(
          Map("$typeName" -> Direct(vfqn, fullyQualifiedNameType)),
          Type
        )
      )
    }
  }

  it should "evaluate single-parameter data type Box$DataType(Int) correctly" in {
    val boxVfqn = ValueFQN(testModuleName, "Box$DataType")
    val intVfqn = ValueFQN(testModuleName, "Int$DataType")

    val boxResolved = createResolvedValue(boxVfqn, Seq("A"))
    val intResolved = createResolvedValue(intVfqn, Seq.empty)

    val intType = Structure(Map("$typeName" -> Direct(intVfqn, fullyQualifiedNameType)), Type)

    runDataTypeEvaluatorWithFacts(boxVfqn, Seq(boxResolved, intResolved)).flatMap {
      case nf: NativeFunction =>
        IO.pure(nf.body(intType))
      case other              =>
        IO.raiseError(new Exception(s"Expected NativeFunction, got: $other"))
    }.asserting { result =>
      result shouldBe ConcreteValue(
        Structure(
          Map(
            "$typeName" -> Direct(boxVfqn, fullyQualifiedNameType),
            "A"         -> intType
          ),
          Type
        )
      )
    }
  }

  it should "evaluate two-parameter data type Either$DataType(Int)(String) correctly" in {
    val eitherVfqn = ValueFQN(testModuleName, "Either$DataType")
    val intVfqn    = ValueFQN(testModuleName, "Int$DataType")
    val stringVfqn = ValueFQN(testModuleName, "String$DataType")

    val eitherResolved = createResolvedValue(eitherVfqn, Seq("A", "B"))
    val intResolved    = createResolvedValue(intVfqn, Seq.empty)
    val stringResolved = createResolvedValue(stringVfqn, Seq.empty)

    val intType    = Structure(Map("$typeName" -> Direct(intVfqn, fullyQualifiedNameType)), Type)
    val stringType = Structure(Map("$typeName" -> Direct(stringVfqn, fullyQualifiedNameType)), Type)

    runDataTypeEvaluatorWithFacts(eitherVfqn, Seq(eitherResolved, intResolved, stringResolved)).flatMap {
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
            "$typeName" -> Direct(eitherVfqn, fullyQualifiedNameType),
            "A"         -> intType,
            "B"         -> stringType
          ),
          Type
        )
      )
    }
  }

  it should "return NativeFunction for partial application Either$DataType(Int)" in {
    val eitherVfqn = ValueFQN(testModuleName, "Either$DataType")
    val intVfqn    = ValueFQN(testModuleName, "Int$DataType")

    val eitherResolved = createResolvedValue(eitherVfqn, Seq("A", "B"))
    val intResolved    = createResolvedValue(intVfqn, Seq.empty)

    val intType = Structure(Map("$typeName" -> Direct(intVfqn, fullyQualifiedNameType)), Type)

    runDataTypeEvaluatorWithFacts(eitherVfqn, Seq(eitherResolved, intResolved)).flatMap {
      case nf: NativeFunction =>
        IO.pure(nf.body(intType))
      case other              =>
        IO.raiseError(new Exception(s"Expected NativeFunction, got: $other"))
    }.asserting { result =>
      result shouldBe a[NativeFunction]
    }
  }

  it should "not generate fact for non-DataType names" in {
    val vfqn = ValueFQN(testModuleName, "someFunction")

    runDataTypeEvaluatorExpectNone(vfqn).asserting(_ shouldBe None)
  }

  it should "reduce applied lambda in signature to zero-parameter type" in {
    // Signature is: (a: Type -> a)(Type) which should reduce to just Type
    // This tests that applications in the signature are properly reduced
    val selfVfqn = ValueFQN(testModuleName, "Self$DataType")

    // Build: (a: Type -> a)(Type)
    val innerLambda = Expression.FunctionLiteral(
      sourced("a"),
      sourced(TypeStack(NonEmptySeq.of(Expression.ValueReference(sourced(typeVfqn))))),
      sourced(TypeStack(NonEmptySeq.of(Expression.ParameterReference(sourced("a")))))
    )
    val appliedExpr = Expression.FunctionApplication(
      sourced(TypeStack(NonEmptySeq.of(innerLambda))),
      sourced(TypeStack(NonEmptySeq.of(Expression.ValueReference(sourced(typeVfqn)))))
    )
    val resolvedValue = ResolvedValue(
      selfVfqn,
      sourced(selfVfqn.name),
      None,
      sourced(TypeStack(NonEmptySeq.of(appliedExpr)))
    )

    runDataTypeEvaluator(selfVfqn, resolvedValue).asserting { result =>
      // Should be ConcreteValue (0 params) since (a -> a)(Type) reduces to Type
      result shouldBe ConcreteValue(
        Structure(
          Map("$typeName" -> Direct(selfVfqn, fullyQualifiedNameType)),
          Type
        )
      )
    }
  }

  it should "not generate fact for Function$DataType (handled by FunctionDataTypeEvaluator)" in {
    val functionVfqn     = ValueFQN(ModuleName.systemFunctionModuleName, "Function$DataType")
    val functionResolved = createResolvedValue(functionVfqn, Seq("A", "B"))

    runDataTypeEvaluatorExpectNoneWithFacts(functionVfqn, Seq(functionResolved)).asserting(_ shouldBe None)
  }

  /** Creates a ResolvedValue with the given type parameters. Signatures are FunctionLiterals ending in Type (not
    * self-reference), matching what CoreProcessor produces.
    */
  private def createResolvedValue(vfqn: ValueFQN, typeParams: Seq[String]): ResolvedValue = {
    // Signature ends in Type (not self-reference)
    val typeExpr = typeParams.foldRight[Expression](Expression.ValueReference(sourced(typeVfqn))) { (param, body) =>
      Expression.FunctionLiteral(
        sourced(param),
        sourced(TypeStack(NonEmptySeq.of(Expression.ValueReference(sourced(typeVfqn))))),
        sourced(TypeStack(NonEmptySeq.of(body)))
      )
    }
    ResolvedValue(
      vfqn,
      sourced(vfqn.name),
      None,
      sourced(TypeStack(NonEmptySeq.of(typeExpr)))
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
    runGeneratorWithFacts(typeEvaluable +: resolvedValues, NamedEvaluable.Key(vfqn)).map {
      case (Some(namedEvaluable), _) => namedEvaluable.value
      case _                         => throw new Exception(s"No NamedEvaluable generated for $vfqn")
    }

  private def runDataTypeEvaluatorExpectNone(vfqn: ValueFQN): IO[Option[NamedEvaluable]] =
    runGeneratorWithFacts(Seq.empty, NamedEvaluable.Key(vfqn)).map(_._1)

  private def runDataTypeEvaluatorExpectNoneWithFacts(
      vfqn: ValueFQN,
      resolvedValues: Seq[ResolvedValue]
  ): IO[Option[NamedEvaluable]] =
    runGeneratorWithFacts(typeEvaluable +: resolvedValues, NamedEvaluable.Key(vfqn)).map(_._1)
}
