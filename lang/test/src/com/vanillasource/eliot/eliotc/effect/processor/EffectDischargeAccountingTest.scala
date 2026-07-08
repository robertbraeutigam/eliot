package com.vanillasource.eliot.eliotc.effect.processor

import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.effect.fact.{EffectCheckedValue, EffectDischargeSummary}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors

/** Regression harness for discharge-aware effect accounting (docs/effect-discharge-accounting.md, Steps 0–3). A callee
  * that *discharges* an effect removes it from the union of its arguments' effects, so a body that fully discharges an
  * internal effect need not declare it — fixing the phantom false positive where `printLine(if(flag,"a") else "b")`
  * forced a spurious `{Abort}`. The discharge is either *declared* (a `{…, -E}` negative member, Steps 0–2) or
  * *inferred* from a user handler's body (Step 3): the parameters' effects that did not survive it.
  *
  * The mechanism is exercised with a synthetic ability `MyE` and dischargers over it, so it is tested directly at the
  * effect phase without the full `Abort`/`AbortCarrier` stack; the real `else` path is covered end-to-end by the
  * `examples/` smoke compiles. The hard invariant — a *genuinely undischarged* effect must stay rejected — is locked
  * below and must never turn green.
  */
class EffectDischargeAccountingTest extends ProcessorTest(LangProcessors()*) {

  // Minimal effect machinery so a snippet naming the carrier resolves (mirrors `stdlib/.../Effect.els`).
  private val effectStub =
    SystemImport(
      "Effect",
      "ability Effect[F[_]] {\ndef flatMap[A, B](f: Function[A, F[B]], fa: F[A]): F[B]\ndef pure[A](a: A): F[A]\ndef map[A, B](f: Function[A, B], fa: F[A]): F[B]\n}",
      ModuleName.effectPackage
    )

  // The `Inf` opt-out effect (import-required), so a `{Inf}` snippet resolves and the "never discharged" case can name it.
  private val infStub =
    SystemImport("Inf", "ability Inf[F[_]] {\ndef forever(step: F[Unit]): F[Unit]\n}", ModuleName.effectPackage)

  // A synthetic discharger surface: `emit` performs `MyE`; `emitBoth` performs `MyE` *and* `Log`; `discharge` is an
  // (abstract) discharger *declaring* `{-MyE}`, so a direct call subtracts `MyE` from its argument's effects.
  // `userDischarge` is a *user handler* that discharges `MyE` by *inference* — no annotation; its `{MyE}` parameter's
  // effect does not survive its body (`discharge` consumes it), so Step 3 infers it discharges `MyE`. `passthru`
  // returns its `{MyE}` parameter unchanged, so `MyE` survives and no discharge is inferred. `Carrier` stands in for the
  // internal `AbortCarrier`-style carrier — types are never checked at the effect phase, so its shape need only be
  // nameable (`userDischarge`'s pure return would trip the declared-pure fail-safe end-to-end — Step 6 — but its
  // discharge *summary*, which callers read, is produced independently of its own check).
  private val dischargeStub =
    SystemImport(
      "Discharge",
      "import eliot.effect.Effect\n" +
        "import eliot.effect.Log\n" +
        "import eliot.effect.Inf\n" +
        "ability MyE[F[_]] {\ndef emit: F[Unit]\n}\n" +
        "def emitBoth: {MyE, Log} Unit\n" +
        "def useMyE: {MyE} String\n" +
        "def combine[A](a: A, b: A): A\n" +
        "type Carrier[G[_], A]\n" +
        "def discharge[G[_] ~ Effect, A](c: Carrier[G, A]): {-MyE} G[A]\n" +
        "def userDischarge[A](x: {MyE} A): A = discharge(x)\n" +
        "def passthru[A](x: {MyE} A): {MyE} A = x\n" +
        // Let-bound discharge (Step 5, investigated — conservative): the `val y = x` bind detaches the effect from the
        // binder's occurrences, so no discharge is inferred even though `discharge(y)` would discharge it in direct style.
        "def userDischargeLet[A](x: {MyE} A): A = {\nval y = x\ndischarge(y)\n}\n" +
        // Double-use: `x` is discharged in one occurrence and used raw in the other, so `MyE` survives → no discharge.
        "def doubleUse[A](x: {MyE} A): A = combine(discharge(x), x)\n" +
        // `discharge` discharges `MyE`, never the `{Inf}` carried by `x`, so `Inf` survives → no discharge.
        "def tryInf[A](x: {Inf} A): {Inf} A = discharge(x)",
      ModuleName.effectPackage
    )

  private val allImports = systemImports :+ effectStub :+ infStub :+ dischargeStub

  "discharge accounting" should "subtract a discharged effect so an honest {Console} passes" in {
    runEffectCheckErrors(
      "import eliot.effect.Console\nimport eliot.effect.Discharge\ndef demo: {Console} Unit = printLine(discharge(emit))"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "still reject an effect that is performed but never discharged" in {
    runEffectCheckErrors(
      "import eliot.effect.Console\nimport eliot.effect.Discharge\ndef bad: {Console} Unit = printLine(emit)"
    ).asserting(
      _.map(_.message) should contain(
        "This value performs the effect 'MyE' but does not declare it; add it to its { ... } effect set."
      )
    )
  }

  it should "subtract only the discharged effect, leaving a sibling effect still required" in {
    runEffectCheckErrors(
      "import eliot.effect.Console\nimport eliot.effect.Discharge\ndef demo: {Console} Unit = printLine(discharge(emitBoth))"
    ).asserting(
      _.map(_.message) should contain(
        "This value performs the effect 'Log' but does not declare it; add it to its { ... } effect set."
      )
    )
  }

  it should "accept when the surviving sibling of a partial discharge is declared" in {
    runEffectCheckErrors(
      "import eliot.effect.Console\nimport eliot.effect.Log\nimport eliot.effect.Discharge\ndef demo: {Console, Log} Unit = printLine(discharge(emitBoth))"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "leave a pure body green (no phantom discharge interaction)" in {
    runEffectCheckErrors("def greet(name: String): String = name").asserting(_ shouldBe Seq.empty)
  }

  "discharge inference (Step 3)" should "record a declared discharger's negative member in its summary" in {
    dischargeSummaryOf("discharge").asserting(_ shouldBe Set("MyE"))
  }

  it should "infer that a user handler discharges the effect its body consumes" in {
    dischargeSummaryOf("userDischarge").asserting(_ shouldBe Set("MyE"))
  }

  it should "infer no discharge for a passthrough that returns its effectful parameter" in {
    dischargeSummaryOf("passthru").asserting(_ shouldBe Set.empty)
  }

  it should "let a caller subtract a user handler's inferred discharge" in {
    runEffectCheckErrors(
      "import eliot.effect.Console\nimport eliot.effect.Discharge\ndef demo: {Console} Unit = printLine(userDischarge(emit))"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "still force the effect on a caller of a non-discharging passthrough" in {
    runEffectCheckErrors(
      "import eliot.effect.Console\nimport eliot.effect.Discharge\ndef demo: {Console} Unit = printLine(passthru(emit))"
    ).asserting(
      _.map(_.message) should contain(
        "This value performs the effect 'MyE' but does not declare it; add it to its { ... } effect set."
      )
    )
  }

  "double / mixed effects (Step 4)" should "infer no discharge when a parameter is also used undischarged (double-use)" in {
    dischargeSummaryOf("doubleUse").asserting(_ shouldBe Set.empty)
  }

  it should "never infer a discharge of Inf (the totality opt-out must keep propagating)" in {
    dischargeSummaryOf("tryInf").asserting(_ shouldBe Set.empty)
  }

  it should "keep an undischarged sibling's effect while another argument is discharged (subtree scoping)" in {
    // `discharge(emit)` discharges MyE in the first argument only; `useMyE` in the second still performs it.
    runEffectCheckErrors(
      "import eliot.effect.Console\nimport eliot.effect.Discharge\ndef demo: {Console} Unit = printLine(combine(discharge(emit), useMyE))"
    ).asserting(
      _.map(_.message) should contain(
        "This value performs the effect 'MyE' but does not declare it; add it to its { ... } effect set."
      )
    )
  }

  it should "leave a discharged argument fully local (a pure sibling forces nothing)" in {
    runEffectCheckErrors(
      "import eliot.effect.Console\nimport eliot.effect.Discharge\ndef demo: {Console} Unit = printLine(combine(discharge(emit), \"ok\"))"
    ).asserting(_ shouldBe Seq.empty)
  }

  // ── Let-binds (Step 5) — investigated, documented limitation ────────────────────────────────────────────────────
  // Let-bound discharge is NOT supported: `val y = e` is an immediately-applied lambda that the *checker* sequences
  // with `Effect.flatMap`, binding `y` to the payload type (`String`), not the carrier — so a later discharger (which
  // needs the carrier) fails at monomorphization with a raw type mismatch. Fixing only the def-local accounting would
  // move that clear def-site error to a worse use-site one; direct style (`printLine(discharge(emit))`, above) is the
  // supported path (docs/effect-discharge-accounting.md, Step 5). These lock the conservative, still-sound behaviour:
  // the accounting never *silently drops* a let-bound effect.

  "let-binds (Step 5)" should "conservatively still require a let-bound effect that a body discharges only via the binder" in {
    // Direct `printLine(discharge(emit))` passes (first test); through a `val` bind the effect is not discharged, so
    // demo genuinely still performs MyE and must declare it — no phantom-free pass, no silent drop.
    runEffectCheckErrors(
      "import eliot.effect.Console\nimport eliot.effect.Discharge\ndef demo: {Console} Unit = {\nval y = emit\nprintLine(discharge(y))\n}"
    ).asserting(
      _.map(_.message) should contain(
        "This value performs the effect 'MyE' but does not declare it; add it to its { ... } effect set."
      )
    )
  }

  it should "infer no discharge for a handler that discharges only through a let binder (conservative)" in {
    dischargeSummaryOf("userDischargeLet").asserting(_ shouldBe Set.empty)
  }

  private def runEffectCheckErrors(source: String) =
    runGenerator(source, EffectCheckedValue.Key(definedVfqn(source)), allImports).map(_._1)

  /** The discharged ability names in a `eliot.effect.Discharge`-module value's [[EffectDischargeSummary]]. */
  private def dischargeSummaryOf(value: String) = {
    val vfqn = ValueFQN(ModuleName(ModuleName.effectPackage, "Discharge"), QualifiedName(value, Qualifier.Default))
    val key  = EffectDischargeSummary.Key(vfqn)
    runGenerator("def probe: String = \"x\"", key, allImports).map { case (_, facts) =>
      facts.get(key) match {
        case Some(summary: EffectDischargeSummary) => summary.dischargedEffects.map(_.abilityName)
        case _                                     => Set.empty[String]
      }
    }
  }

  /** The single value defined by each snippet (the name after the first `def`, before `:`/`(`). */
  private def definedVfqn(source: String): ValueFQN = {
    val name = source.linesIterator
      .map(_.trim)
      .find(_.startsWith("def "))
      .map(_.drop(4).takeWhile(c => c != ':' && c != '(' && c != ' '))
      .getOrElse("demo")
    ValueFQN(testModuleName, QualifiedName(name, Qualifier.Default))
  }
}
