package com.vanillasource.eliot.eliotc.resolve.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue

/** Row aliases (`type Web = {Console, Log}`, docs/effects-v5-one-carrier.md §7): a name for a *set of abilities*,
  * expanded where a row entry names it.
  *
  * The expansion is a resolve-phase rewrite, so both channels a row feeds are asserted here — the declared row
  * ([[ResolvedValue.effectRow]], the verifiers' vocabulary) and the carrier's `~` constraints (what the desugared
  * signature actually says) — since a change reaching only one of them would silently drop the effect from the other.
  */
class RowAliasTest extends ProcessorTest(LangProcessors()*) {

  "a row alias" should "expand into its entries in the declared row" in {
    declaredRow("type Web = {Console, Log}\ndef f: {Web} String = \"\"")
      .asserting(_ shouldBe Seq("Console", "Log"))
  }

  it should "expand into the carrier's ability constraints too" in {
    carrierConstraints("type Web = {Console, Log}\ndef f: {Web} String = \"\"")
      .asserting(_ shouldBe Seq("Console", "Log"))
  }

  it should "produce exactly what writing the entries out produces" in {
    (
      declaredRow("type Web = {Console, Log}\ndef f: {Web} String = \"\""),
      declaredRow("def f: {Console, Log} String = \"\"")
    ).mapN(_ shouldBe _)
  }

  it should "join what a sibling entry names, collapsing the overlap" in {
    declaredRow("type Web = {Console, Log}\ndef f: {Web, Console} String = \"\"")
      .asserting(_ shouldBe Seq("Console", "Log"))
  }

  it should "expand transitively through another alias" in {
    declaredRow("type Loud = {Console, Log}\ntype Web = {Loud, Abort}\ndef f: {Web} String = \"\"")
      .asserting(_ shouldBe Seq("Console", "Log", "Abort"))
  }

  it should "substitute its parameters with the use's type arguments" in {
    entryArguments("type Fallible[E] = {Throw[E], Console}\ndef f: {Fallible[String]} String = \"\"")
      .asserting(_ shouldBe Seq(("Throw", Seq("eliot.lang.String::String^Type")), ("Console", Seq.empty)))
  }

  it should "expand in a parameter position as well as a return one" in {
    declaredRow("type Web = {Console, Log}\ndef f(action: String => {Web} String): String = action", parameterRow = true)
      .asserting(_ shouldBe Seq("Console", "Log"))
  }

  it should "be rejected when used with the wrong number of type arguments" in {
    errors("type Fallible[E] = {Throw[E]}\ndef f: {Fallible} String = \"\"")
      .asserting(_ should contain("Row alias 'Fallible' takes 1 type argument(s), but got 0."))
  }

  it should "be rejected when it names itself, directly or through another alias" in {
    errors("type A = {B, Console}\ntype B = {A, Log}\ndef f: {A} String = \"\"")
      .asserting(_ should contain("Row alias is defined recursively."))
  }

  it should "leave an unknown row entry the ordinary unknown-ability error" in {
    errors("def f: {Nonesuch} String = \"\"").asserting(_ should contain("Ability not found."))
  }

  it should "lose to an ability of the same name, which is what a row entry means first" in {
    declaredRow("ability Web[F[_]] { def ping: {Web} String }\ntype Web = {Console}\ndef f: {Web} String = \"\"")
      .asserting(_ shouldBe Seq("Web"))
  }

  private def declaredRow(source: String, parameterRow: Boolean = false): IO[Seq[String]] =
    resolvedValue(source).map { rv =>
      val row = if (parameterRow) rv.effectRow.parameterEffects.flatMap(_.effects) else rv.effectRow.returnEffects
      row.map(_.abilityFQN.abilityName).filterNot(_ == "Effect")
    }

  private def carrierConstraints(source: String): IO[Seq[String]] =
    resolvedValue(source).map(
      _.paramConstraints.values.flatten.map(_.abilityFQN.abilityName).filterNot(_ == "Effect").toSeq
    )

  private def entryArguments(source: String): IO[Seq[(String, Seq[String])]] =
    resolvedValue(source).map(
      _.effectRow.returnEffects.map(c => (c.abilityFQN.abilityName, c.typeArgs.map(_.render)))
    )

  private def errors(source: String): IO[Seq[String]] =
    runGenerator(source, ResolvedValue.Key(fVfqn), systemImports).map(_._1.map(_.message).distinct)

  private def resolvedValue(source: String): IO[ResolvedValue] =
    runGenerator(source, ResolvedValue.Key(fVfqn), systemImports).map { case (errors, facts) =>
      if (errors.nonEmpty) throw new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}")
      facts.values.collectFirst { case rv: ResolvedValue if rv.vfqn == fVfqn => rv }.get
    }

  private val fVfqn = ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default))
}
