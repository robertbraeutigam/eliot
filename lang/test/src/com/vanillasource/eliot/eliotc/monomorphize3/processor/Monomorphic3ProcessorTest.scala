package com.vanillasource.eliot.eliotc.monomorphize3.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.eval.processor.{
  DataTypeEvaluator,
  ExistingNamedValueEvaluator,
  SystemValueEvaluator
}
import com.vanillasource.eliot.eliotc.implementation.processor.{
  AbilityImplementationCheckProcessor,
  AbilityImplementationProcessor
}
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.monomorphize3.fact.{GroundValue, Monomorphic3Expression, Monomorphic3Value}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

class Monomorphic3ProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      SystemValueEvaluator(),
      ExistingNamedValueEvaluator(),
      DataTypeEvaluator(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(
        Seq(
          ModuleName.systemFunctionModuleName,
          ModuleName(ModuleName.defaultSystemPackage, "Number"),
          ModuleName(ModuleName.defaultSystemPackage, "String"),
          ModuleName(ModuleName.defaultSystemPackage, "BigInteger")
        )
      ),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor(),
      OperatorResolverProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      SystemNativesProcessor(),
      DataTypeNativesProcessor(),
      UserValueNativesProcessor(),
      Monomorphic3Processor()
    ) {

  override val systemImports: Seq[SystemImport] = Seq(
    SystemImport("Function", "opaque type Function[A, B]"),
    SystemImport("Type", "opaque type Type"),
    SystemImport("Number", "opaque type Int"),
    SystemImport("String", "opaque type String"),
    SystemImport("BigInteger", "opaque type BigInteger")
  )

  private def dummySourced[T](v: T) = Sourced[T](file, PositionRange.zero, v)

  "Monomorphic3Processor" should "monomorphize non-generic value" in {
    runEngineForMonomorphicValue("def f: BigInteger")
      .asserting { result =>
        showType(result.signature) shouldBe "BigInteger"
        result.runtime shouldBe None
      }
  }

  it should "monomorphize function literal in body" in {
    runEngineForMonomorphicValue("def f: Function[BigInteger, BigInteger] = (x: BigInteger) -> x")
      .asserting(_.runtime.get.value shouldBe a[Monomorphic3Expression.FunctionLiteral])
  }

  it should "monomorphize integer literal in body" in {
    runEngineForMonomorphicValue("def f: BigInteger = 42")
      .asserting(_.runtime.get.value shouldBe a[Monomorphic3Expression.IntegerLiteral])
  }

  it should "monomorphize string literal in body" in {
    runEngineForMonomorphicValue("def f: String = \"hello\"")
      .asserting(_.runtime.get.value shouldBe a[Monomorphic3Expression.StringLiteral])
  }

  it should "monomorphize value reference to non-generic value" in {
    runEngineForMonomorphicValue("def constVal: BigInteger\ndef f: BigInteger = constVal")
      .asserting(_.runtime.get.value shouldBe a[Monomorphic3Expression.MonomorphicValueReference])
  }

  private def runEngineForMonomorphicValue(
      source: String,
      name: String = "f",
      typeArgs: Seq[Sourced[OperatorResolvedExpression]] = Seq.empty
  ): IO[Monomorphic3Value] =
    runGenerator(
      source,
      Monomorphic3Value.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      systemImports
    ).flatMap { case (errors, facts) =>
      if (errors.nonEmpty)
        IO.raiseError(new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}"))
      else
        facts.values.collectFirst { case v: Monomorphic3Value if v.vfqn.name.name == name => v } match {
          case Some(v) => IO.pure(v)
          case None    => IO.raiseError(new Exception(s"No Monomorphic3Value found for '$name'"))
        }
    }

  private def showType(value: GroundValue): String = value match {
    case GroundValue.Structure(fields, GroundValue.Type) =>
      val typeName = fields("$typeName").asInstanceOf[GroundValue.Direct].value.asInstanceOf[ValueFQN].name.name
      val typeArgs = fields.removed("$typeName")
      if (typeArgs.isEmpty) typeName
      else s"$typeName[${typeArgs.toSeq.sortBy(_._1).map(f => showType(f._2)).mkString(", ")}]"
    case GroundValue.Type                                => "Type"
    case _                                               => value.toString
  }
}
