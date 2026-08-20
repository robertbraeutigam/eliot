package com.vanillasource.eliot.eliotc.resolve.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue

/** An ability may require other abilities of its carrier (`ability Web[F[_] ~ Console & Log]`), and a use of it
  * declares what it requires — the superability relation, and the one rule that lets a name stand for a set of
  * effects (docs/effects-v5-one-carrier.md §7).
  *
  * The constraints on a value's carrier binder are what *both* verifiers read as "declared"
  * (`RowChecker.declaredRow`, `EffectAccountingProcessor.openRow`), so that is what these assert.
  */
class AbilityRequirementTest extends ProcessorTest(LangProcessors()*) {

  "a required ability" should "be declared by a row entry naming the ability that requires it" in {
    carrierConstraints("ability Web[F[_] ~ Console & Log]\ndef f: {Web} String = \"\"")
      .asserting(_ shouldBe Seq("Web", "Console", "Log"))
  }

  it should "declare the same set as writing the entries out, plus the name itself" in {
    carrierConstraints("ability Web[F[_] ~ Console & Log]\ndef f: {Web} String = \"\"")
      .asserting(_.toSet.diff(Set("Web")) shouldBe Set("Console", "Log"))
  }

  it should "collapse what a sibling entry already names" in {
    carrierConstraints("ability Web[F[_] ~ Console & Log]\ndef f: {Web, Console} String = \"\"")
      .asserting(_ shouldBe Seq("Web", "Console", "Log"))
  }

  it should "close transitively through another requiring ability" in {
    carrierConstraints(
      "ability Loud[F[_] ~ Console & Log]\nability Web[F[_] ~ Loud & Abort]\ndef f: {Web} String = \"\""
    ).asserting(_ shouldBe Seq("Web", "Loud", "Abort", "Console", "Log"))
  }

  it should "substitute the ability's parameters with the arguments the use wrote" in {
    constraintArguments(
      "ability Fallible[E, F[_] ~ Throw[E, F] & Console]\ndef f: {Fallible[String]} String = \"\""
    ).asserting(
      _ shouldBe Seq(
        ("Fallible", Seq("eliot.lang.String::String^Type", "F")),
        ("Throw", Seq("eliot.lang.String::String^Type", "F")),
        ("Console", Seq("F"))
      )
    )
  }

  it should "be inherited by a hand-written `~` constraint too, not just a row" in {
    carrierConstraints("ability Web[F[_] ~ Console & Log]\ndef f[G[_] ~ Web](g: G[String]): String = \"\"", "G")
      .asserting(_ shouldBe Seq("Web", "Console", "Log"))
  }

  it should "stay on the parameter it was declared for, never landing on an unrelated binder" in {
    // `Show` is required of `Fallible`'s *first* parameter, which the use binds to `String` — not to the carrier,
    // so it must not join the carrier's constraints (where it would read as a declared effect).
    carrierConstraints(
      "ability Fallible[E ~ Show, F[_] ~ Throw[E, F]]\ndef f: {Fallible[String]} String = \"\""
    ).asserting(_ shouldBe Seq("Fallible", "Throw"))
  }

  it should "close rather than loop when two abilities require each other" in {
    carrierConstraints(
      "ability A[F[_] ~ B & Console]\nability B[F[_] ~ A & Log]\ndef f: {A} String = \"\""
    ).asserting(_.toSet shouldBe Set("A", "B", "Console", "Log"))
  }

  it should "leave an ability that requires nothing exactly as it was" in {
    carrierConstraints("def f: {Console} String = \"\"").asserting(_ shouldBe Seq("Console"))
  }

  private def carrierConstraints(source: String, binder: String = "F"): IO[Seq[String]] =
    resolvedValue(source).map(
      _.paramConstraints.getOrElse(binder, Seq.empty).map(_.abilityFQN.abilityName).filterNot(_ === "Effect")
    )

  private def constraintArguments(source: String, binder: String = "F"): IO[Seq[(String, Seq[String])]] =
    resolvedValue(source).map(
      _.paramConstraints
        .getOrElse(binder, Seq.empty)
        .filterNot(_.abilityFQN.abilityName === "Effect")
        .map(c => (c.abilityFQN.abilityName, c.typeArgs.map(_.render)))
    )

  private def resolvedValue(source: String): IO[ResolvedValue] =
    runGenerator(source, ResolvedValue.Key(fVfqn), systemImports).map { case (errors, facts) =>
      if (errors.nonEmpty) throw new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}")
      facts.values.collectFirst { case rv: ResolvedValue if rv.vfqn === fVfqn => rv }.get
    }

  private val fVfqn = ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default))
}
