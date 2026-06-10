package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ability.processor.{
  AbilityImplementationCheckProcessor,
  AbilityImplementationProcessor,
  ModuleAbilityOverlapCheckProcessor
}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer

/** Verifies that [[MatchNativesProcessor]] makes the NbE evaluator reduce `match` on a concrete scrutinee to a ground
  * value — the `interpret`-backend oracle cases, re-expressed at the NbE level (plan P1).
  */
class MatchNativesProcessorTest
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
      MatchNativesProcessor(),
      UserValueNativesProcessor()
    ) {

  override val systemImports = Seq(
    SystemImport("Function", "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"),
    SystemImport("Type", "type Type"),
    SystemImport("BigInteger", "type BigInteger"),
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

  "match natives" should "reduce a data-match on nullary constructors (handleCases)" in {
    groundOf(
      "data Boolean = True | False\n" +
        "def negate(b: Boolean): Boolean = b match { case True -> False case False -> True }\n" +
        "def result: Boolean = negate(True)",
      "result"
    ).asserting(_.collect { case GroundValue.Structure(name, _, _) => name } shouldBe Some(constructorFqn("False")))
  }

  it should "reduce a data-match binding a constructor field (handleCases)" in {
    groundOf(
      "data Box(content: String)\n" +
        "def unwrap(b: Box): String = b match { case Box(x) -> x }\n" +
        "def result: String = unwrap(Box(\"hi\"))",
      "result"
    ).asserting(_.collect { case GroundValue.Direct(value, _) => value } shouldBe Some("hi"))
  }

  it should "reduce a type-match selecting the matching constructor (typeMatch)" in {
    groundOf(
      "data Tag[NAME: String](content: String)\n" +
        "def tagName(t: Type): String = t match { case Tag[name] -> \"matched\" case _ -> \"untagged\" }\n" +
        "def result: String = tagName(Tag[\"hello\"])",
      "result"
    ).asserting(_.collect { case GroundValue.Direct(value, _) => value } shouldBe Some("matched"))
  }

  it should "reduce a type-match falling through to the wildcard (typeMatch)" in {
    groundOf(
      "data Tag[NAME: String](content: String)\ndata Other[X: String](payload: String)\n" +
        "def tagName(t: Type): String = t match { case Tag[name] -> \"matched\" case _ -> \"untagged\" }\n" +
        "def result: String = tagName(Other[\"hello\"])",
      "result"
    ).asserting(_.collect { case GroundValue.Direct(value, _) => value } shouldBe Some("untagged"))
  }

  private def constructorFqn(name: String): ValueFQN = ValueFQN(testModuleName, default(name))

  /** Evaluate the named closed value's NbE binding to a [[GroundValue]] via [[Evaluator.semToGround]]. */
  private def groundOf(source: String, name: String): IO[Option[GroundValue]] = {
    val key = NativeBinding.Key(ValueFQN(testModuleName, default(name)))
    runGenerator(source, key, systemImports)
      .map(_._2.get(key).map(fact => Evaluator.semToGround(fact.asInstanceOf[NativeBinding].semValue)))
  }
}
