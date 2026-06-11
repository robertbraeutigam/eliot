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
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer

/** Verifies the unifier's TypeRefinement hook: when two types built from the same constructor are unified and that
  * constructor has a custom `TypeRefinement` implementation, assignability is decided by running that implementation
  * through the NbE evaluator (plan P6) rather than by structural equality. A type with no custom implementation falls
  * back to structural equality.
  */
class RefinementUnifyTest
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
      UserValueNativesProcessor(),
      MonomorphicTypeCheckProcessor()
    ) {

  override val systemImports = Seq(
    SystemImport("Function", "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"),
    SystemImport("Type", "type Type"),
    SystemImport(
      "BigInteger",
      "import eliot.lang.Bool\ntype BigInteger\ndef lessThanOrEqual(a: BigInteger, b: BigInteger): Bool"
    ),
    SystemImport("Unit", "type Unit"),
    SystemImport("String", "type String"),
    SystemImport("IO", "type IO"),
    SystemImport("Bool", ProcessorTest.boolImportContent),
    SystemImport(
      "PatternMatch",
      "ability PatternMatch[T] {\ntype Cases[R]\ndef handleCases[R](value: T, cases: Cases[R]): R\n}"
    ),
    SystemImport(
      "TypeMatch",
      "ability TypeMatch[T] {\ntype Fields[R]\ndef typeMatch[R](value: Type, matched: Fields[R], notMatched: Function[Unit, R]): R\n}"
    ),
    SystemImport("TypeRefinement", ProcessorTest.typeRefinementImportContent)
  )

  private val imports = "import eliot.lang.Bool\nimport eliot.lang.TypeRefinement\n"

  "the refinement hook" should "accept a structurally-mismatching assignment when the custom impl returns true" in {
    // Box[String] is assigned where Box[BigInteger] is expected. Structural equality would reject (String != BigInteger);
    // the custom TypeRefinement[Box] returns true, so the hook accepts.
    errorsOf(
      imports +
        "data Box[A]\n" +
        "implement TypeRefinement[Box] { def assignableFrom(target: Type, source: Type): Bool = true }\n" +
        "def f(x: Box[String]): Box[BigInteger] = x"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "reject a structurally-matching assignment when the custom impl returns false" in {
    // Box[String] assigned where Box[String] is expected — structurally equal — but the custom impl returns false.
    errorsOf(
      imports +
        "data Box[A]\n" +
        "implement TypeRefinement[Box] { def assignableFrom(target: Type, source: Type): Bool = false }\n" +
        "def f(x: Box[String]): Box[String] = x"
    ).asserting(_ should contain("Type mismatch."))
  }

  it should "fall back to structural equality when the constructor has no custom impl" in {
    errorsOf(
      imports +
        "data Box[A]\n" +
        "def f(x: Box[String]): Box[BigInteger] = x"
    ).asserting(_ should contain("Type mismatch."))
  }

  it should "accept a structurally-matching assignment with no custom impl (control)" in {
    errorsOf(
      imports +
        "data Box[A]\n" +
        "def f(x: Box[String]): Box[String] = x"
    ).asserting(_ shouldBe Seq.empty)
  }

  // A realistic range refinement, the same shape the eventual Int[MIN, MAX] uses: assignableFrom runs a nested
  // type-match plus lessThanOrEqual/&& through the NbE evaluator. Ranged[0,10] widens to Ranged[0,100] (accepted);
  // Ranged[0,100] does not narrow to Ranged[0,10] (rejected). (A `data` type is used because matching on a constructor
  // currently requires TypeMatch, which abstract `type` declarations like the real Int do not yet get.)
  private val rangedRefinement =
    "data Ranged[LO: BigInteger, HI: BigInteger]\n" +
      "implement TypeRefinement[Ranged] {\n" +
      "  def assignableFrom(target: Type, source: Type): Bool =\n" +
      "    target match {\n" +
      "      case Ranged[tlo, thi] -> source match {\n" +
      "        case Ranged[slo, shi] -> lessThanOrEqual(tlo, slo) && lessThanOrEqual(shi, thi)\n" +
      "        case _ -> false\n" +
      "      }\n" +
      "      case _ -> false\n" +
      "    }\n" +
      "}\n"

  it should "accept widening of a range via a nested-match/comparison refinement" in {
    errorsOf(imports + rangedRefinement + "def f(x: Ranged[0, 10]): Ranged[0, 100] = x")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "reject narrowing of a range via a nested-match/comparison refinement" in {
    errorsOf(imports + rangedRefinement + "def f(x: Ranged[0, 100]): Ranged[0, 10] = x")
      .asserting(_ should contain("Type mismatch."))
  }

  it should "accept an equal range via a nested-match/comparison refinement" in {
    errorsOf(imports + rangedRefinement + "def f(x: Ranged[0, 10]): Ranged[0, 10] = x")
      .asserting(_ shouldBe Seq.empty)
  }

  private def errorsOf(source: String): IO[Seq[String]] = {
    val key = MonomorphicValue.Key(ValueFQN(testModuleName, default("f")), Seq.empty)
    runGenerator(source, key, systemImports).map(_._1.map(_.message))
  }
}
