package com.vanillasource.eliot.eliotc.effect.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.effect.fact.EffectCheckedValue
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors

/** The effect phase's two definition-local diagnostics (docs/effect-lift-in-checker.md, Step 4 — the phase verifies,
  * the checker lifts): the declared-effects subset check (+ `Inf` propagation) and the "declared pure but performs
  * effects" fail-safe. The former rewrite-shape assertions live on as monomorphic-body assertions in
  * `MonomorphicTypeCheckTest` (the lift happens in the checker now).
  */
class EffectCheckProcessorTest extends ProcessorTest(LangProcessors()*) {

  "effect checks" should "accept a pure function body" in {
    runEffectCheckErrors("def greet(name: String): String = name").asserting(_ shouldBe Seq.empty)
  }

  it should "reject an effectful body declared under a pure (non-carrier) return type" in {
    runEffectCheckErrors("import eliot.effect.Console\ndef helper: String = printLine(readLine)")
      .asserting(_.map(_.message) should contain("This value performs an effect but is declared pure; declare an effect set with { ... } or return an effect carrier."))
  }

  it should "reject an effect performed but not declared in the effect set (propagation/subset check)" in {
    runEffectCheckErrors("import eliot.effect.Console\nimport eliot.effect.Log\ndef bad: {Console} Unit = log(readLine)")
      .asserting(_.map(_.message) should contain("This value performs the effect 'Log' but does not declare it; add it to its { ... } effect set."))
  }

  it should "accept a body whose performed effects are all declared" in {
    runEffectCheckErrors("import eliot.effect.Console\nimport eliot.effect.Log\ndef ok: {Console, Log} Unit = log(readLine)").asserting(_ shouldBe Seq.empty)
  }

  it should "propagate the Inf effect: reject a {Console} body that calls forever (undeclared Inf)" in {
    runEffectCheckErrors("import eliot.effect.Console\nimport eliot.effect.Inf\ndef bad: {Console} Unit = forever(printLine(readLine))")
      .asserting(_.map(_.message) should contain("This value performs the effect 'Inf' but does not declare it; add it to its { ... } effect set."))
  }

  it should "accept a {Console, Inf} body that calls forever (Inf declared)" in {
    runEffectCheckErrors("import eliot.effect.Console\nimport eliot.effect.Inf\ndef ok: {Console, Inf} Unit = forever(printLine(readLine))")
      .asserting(_ shouldBe Seq.empty)
  }

  // The Effect ability stub (matching `stdlib/.../Effect.els`), so a snippet naming the machinery resolves.
  private val effectStub =
    SystemImport(
      "Effect",
      "ability Effect[F[_]] {\ndef flatMap[A, B](f: Function[A, F[B]], fa: F[A]): F[B]\ndef pure[A](a: A): F[A]\ndef map[A, B](f: Function[A, B], fa: F[A]): F[B]\n}",
      ModuleName.effectPackage
    )
  // The Inf effect ability stub (matching `stdlib/.../Inf.els`), import-required (not ambient), so the propagation
  // cases above can name `forever` and `{Inf}`.
  private val infStub    =
    SystemImport("Inf", "ability Inf[F[_]] {\ndef forever(step: F[Unit]): F[Unit]\n}", ModuleName.effectPackage)
  private val allImports = systemImports :+ effectStub :+ infStub

  private def runEffectCheckErrors(source: String) =
    runGenerator(source, EffectCheckedValue.Key(definedVfqn(source)), allImports).map(_._1)

  /** The single value defined by each one-liner source (the name after the first `def`, before `:`/`(`). */
  private def definedVfqn(source: String): ValueFQN = {
    val name = source.linesIterator
      .map(_.trim)
      .find(_.startsWith("def "))
      .map(_.drop(4).takeWhile(c => c != ':' && c != '(' && c != ' '))
      .getOrElse("echo")
    ValueFQN(testModuleName, QualifiedName(name, Qualifier.Default))
  }
}
