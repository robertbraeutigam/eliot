package com.vanillasource.eliot.eliotc.saturate

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ability.processor.{
  AbilityImplementationCheckProcessor,
  AbilityImplementationProcessor,
  ModuleAbilityOverlapCheckProcessor
}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.effect.processor.EffectDesugaringProcessor
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.saturate.processor.SaturatedValueProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer

/** Targeted test for the per-binder role analysis ([[com.vanillasource.eliot.eliotc.saturate.fact.BinderRoles]]) carried
  * on [[SaturatedValue]] (D6 of the monomorphize architecture review).
  *
  * D6 extracts the "which leading type-stack binder does the body reify (reference in value position)" classification
  * into one home, computed on the *saturated* signature so the binder indices line up with the type arguments the
  * checker applies. The cases pin: a reified binder, a non-reified ordinary generic, reification through a nested value
  * reference's type argument, and — the alignment win — a value that both auto-saturates a value parameter *and* reifies
  * an explicit generic, where the reified prefix must cover the *saturated* binder list (the unsaturated signature would
  * have only the explicit generic, mis-aligning the binding wrap against the checker's type arguments).
  */
class BinderRolesTest
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
      EffectDesugaringProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      ModuleAbilityOverlapCheckProcessor(),
      SaturatedValueProcessor()
    ) {

  "a type-stack binder referenced in value position" should "be classified reified" in {
    rolesOf("def bigOf[V: BigInteger]: BigInteger = V", "bigOf")
      .asserting(_ shouldBe (Seq.empty, Seq(("V", true))))
  }

  "an ordinary generic never used in value position" should "be classified non-reified" in {
    rolesOf("def id[A](x: A): A = x", "id")
      .asserting(_ shouldBe (Seq.empty, Seq(("A", false))))
  }

  "a binder reified through a nested value reference's type argument" should "be classified reified" in {
    rolesOf("def bigOf[V: BigInteger]: BigInteger = V\ndef h[N: BigInteger]: BigInteger = bigOf[N]", "h")
      .asserting(_ shouldBe (Seq.empty, Seq(("N", true))))
  }

  // The alignment win: `x: Int` saturates to `Int[$Int$0, $Int$1]`, prepending two synthesized binders before the
  // explicit reified `V`. The analysis runs on the saturated signature, so the roles (and the wrapped prefix) span all
  // three binders in checker order — the unsaturated signature would expose only `V`, mis-aligning the binding wrap.
  "a value that auto-saturates a parameter and reifies an explicit generic" should "classify the saturated binders" in {
    rolesOf("def tagged[V: BigInteger](x: Int): BigInteger = V", "tagged")
      .asserting(_ shouldBe (Seq.empty, Seq(("$Int$0", false), ("$Int$1", false), ("V", true))))
  }

  /** Drive the pipeline to a value's [[SaturatedValue]] and project its binder roles to `(name, reified)` pairs in
    * declaration order, alongside any compilation errors.
    */
  private def rolesOf(source: String, name: String): IO[(Seq[TestError], Seq[(String, Boolean)])] = {
    val vfqn = ValueFQN(testModuleName, default(name))
    runGenerator(source, SaturatedValue.Key(vfqn), binderRolesImports).map { case (errors, facts) =>
      val roles = facts
        .get(SaturatedValue.Key(vfqn))
        .collect { case sv: SaturatedValue => sv.binderRoles.roles }
        .getOrElse(Seq.empty)
        .map(role => (role.name.value, role.reified))
      (toTestErrors(errors), roles)
    }
  }

  /** Ambient stubs plus `BigInteger`; `Int`/`Runtime`/`Console`/`Log`/`Dep` are in `defaultSystemModules`, so their
    * stubs must be present even though these scenarios only declare on `BigInteger`/`Int`.
    */
  private val binderRolesImports: Seq[SystemImport] = Seq(
    SystemImport("Function", "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"),
    SystemImport("Type", "type Type"),
    SystemImport("BigInteger", "type BigInteger"),
    SystemImport("Unit", "type Unit"),
    SystemImport("String", "type String"),
    SystemImport("IO", "type IO"),
    SystemImport("PatternMatch", ""),
    SystemImport("TypeMatch", ""),
    SystemImport("Int", ProcessorTest.intStubContent),
    SystemImport("Runtime", ProcessorTest.runtimeStubContent),
    SystemImport("Console", ProcessorTest.consoleStubContent),
    SystemImport("Log", ProcessorTest.logStubContent),
    SystemImport("Dep", ProcessorTest.depStubContent)
  )
}
