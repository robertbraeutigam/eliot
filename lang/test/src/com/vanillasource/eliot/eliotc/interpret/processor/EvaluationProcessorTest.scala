package com.vanillasource.eliot.eliotc.interpret.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ability.processor.{
  AbilityImplementationCheckProcessor,
  AbilityImplementationProcessor,
  ModuleAbilityOverlapCheckProcessor
}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.interpret.fact.EvaluatedValue
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.processor.{
  DataTypeNativesProcessor,
  MonomorphicTypeCheckProcessor,
  SystemNativesProcessor,
  UserValueNativesProcessor
}
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.uncurry.processor.MonomorphicUncurryingProcessor
import com.vanillasource.eliot.eliotc.used.UsedNamesProcessor

class EvaluationProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor(),
      OperatorResolverProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      ModuleAbilityOverlapCheckProcessor(),
      SystemNativesProcessor(),
      DataTypeNativesProcessor(),
      UserValueNativesProcessor(),
      MonomorphicTypeCheckProcessor(),
      UsedNamesProcessor(),
      MonomorphicUncurryingProcessor(),
      EvaluationProcessor()
    ) {

  override val systemImports = Seq(
    SystemImport("Function", "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"),
    SystemImport("Type", "type Type"),
    SystemImport("BigInteger", "type BigInteger\ndef inc(bi: BigInteger): BigInteger"),
    SystemImport("Unit", "type Unit"),
    SystemImport("String", "type String"),
    SystemImport("IO", "type IO"),
    SystemImport("PatternMatch", ""),
    SystemImport("TypeMatch", "")
  )

  private val bigIntegerType: GroundValue =
    GroundValue.Structure(WellKnownTypes.bigIntFQN, Seq.empty, GroundValue.Type)

  "evaluation backend" should "evaluate a nullary value that applies a native" in {
    runEval("def two: BigInteger = inc(1)", "two")
      .asserting(_ shouldBe Some(GroundValue.Direct(BigInt(2), bigIntegerType)))
  }

  it should "evaluate a chain of native applications" in {
    runEval("def three: BigInteger = inc(inc(1))", "three")
      .asserting(_ shouldBe Some(GroundValue.Direct(BigInt(3), bigIntegerType)))
  }

  it should "evaluate through a user-defined function call" in {
    runEval("def addOne(x: BigInteger): BigInteger = inc(x)\ndef four: BigInteger = addOne(inc(inc(1)))", "four")
      .asserting(_ shouldBe Some(GroundValue.Direct(BigInt(4), bigIntegerType)))
  }

  private def runEval(source: String, name: String): IO[Option[GroundValue]] = {
    val key = EvaluatedValue.Key(ValueFQN(testModuleName, default(name)), Seq.empty, Seq.empty)
    runGenerator(source, key, systemImports)
      .map(_._2.get(key).map(_.asInstanceOf[EvaluatedValue].result))
  }
}
