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
    SystemImport(
      "PatternMatch",
      "ability PatternMatch[T] {\ntype Cases[R]\ndef handleCases[R](value: T, cases: Cases[R]): R\n}"
    ),
    SystemImport(
      "TypeMatch",
      "ability TypeMatch[T] {\ntype Fields[R]\ndef typeMatch[R](value: Type, matched: Fields[R], notMatched: Function[Unit, R]): R\n}"
    )
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

  it should "evaluate a data-match on nullary constructors (handleCases)" in {
    runEval(
      "data Switch = On | Off\n" +
        "def toggle(s: Switch): Switch = s match { case On -> Off case Off -> On }\n" +
        "def result: Switch = toggle(On)",
      "result"
    ).asserting(_.collect { case GroundValue.Structure(name, _, _) => name } shouldBe Some(constructorFqn("Off")))
  }

  it should "evaluate a data-match binding a constructor field (handleCases)" in {
    runEval(
      "data Box(content: BigInteger)\n" +
        "def unwrap(b: Box): BigInteger = b match { case Box(x) -> x }\n" +
        "def result: BigInteger = unwrap(Box(inc(1)))",
      "result"
    ).asserting(_ shouldBe Some(GroundValue.Direct(BigInt(2), bigIntegerType)))
  }

  it should "evaluate a type-match selecting the matching constructor (typeMatch)" in {
    runEval(
      "data Tag[NAME: String](content: String)\n" +
        "def tagName(t: Type): String = t match { case Tag[name] -> name case _ -> \"untagged\" }\n" +
        "def result: String = tagName(Tag[\"hello\"])",
      "result"
    ).asserting(_.collect { case GroundValue.Direct(value, _) => value } shouldBe Some("hello"))
  }

  it should "evaluate a type-match falling through to the wildcard (typeMatch)" in {
    runEval(
      "data Tag[NAME: String](content: String)\ndata Other[X: String](payload: String)\n" +
        "def tagName(t: Type): String = t match { case Tag[name] -> name case _ -> \"untagged\" }\n" +
        "def result: String = tagName(Other[\"hello\"])",
      "result"
    ).asserting(_.collect { case GroundValue.Direct(value, _) => value } shouldBe Some("untagged"))
  }

  private def constructorFqn(name: String): ValueFQN = ValueFQN(testModuleName, default(name))

  private def runEval(source: String, name: String): IO[Option[GroundValue]] = {
    val key = EvaluatedValue.Key(ValueFQN(testModuleName, default(name)), Seq.empty, Seq.empty)
    runGenerator(source, key, systemImports)
      .map(_._2.get(key).map(_.asInstanceOf[EvaluatedValue].result))
  }
}
