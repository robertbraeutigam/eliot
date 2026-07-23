package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.plugin.LangProcessors

/** Effects-as-channel Phase 3 §5 — the post-monomorphization **effect accounting** ([[EffectAccountingProcessor]]),
  * the real `derived ⊆ declared` verification path under `--effect-channel` (replacing the Phase-2 in-checker shadow).
  * This suite drives the accounting fact directly for a value and pins: the derived row an effectful body produces,
  * that a declared effect propagates from callee to caller, that over-declaration is accepted (subset, not equality),
  * and that an *undeclared* effect is rejected with the friendly effect-vocabulary diagnostic.
  */
class EffectAccountingTest extends ProcessorTest(LangProcessors(effectChannel = true)*) {

  "effect accounting" should "derive the row an effectful body performs and accept it when declared" in {
    account("def main: {Console} Unit = printLine(\"hello\")")
      .asserting(result => (result._1, result._2.map(_.derivedRow.map(_.abilityName))) shouldBe (Seq.empty, Some(Set("Console"))))
  }

  it should "accept an over-declared row (derived is a proper subset of declared)" in {
    account("def main: {Console, Log} Unit = printLine(\"hello\")")
      .asserting(result => (result._1, result._2.map(_.derivedRow.map(_.abilityName))) shouldBe (Seq.empty, Some(Set("Console"))))
  }

  it should "propagate a callee's declared effect to the caller and accept it when the caller declares it too" in {
    account("def greet: {Console} Unit = printLine(\"hi\")\ndef main: {Console} Unit = greet")
      .asserting(result => (result._1, result._2.map(_.derivedRow.map(_.abilityName))) shouldBe (Seq.empty, Some(Set("Console"))))
  }

  it should "reject a value that performs an undeclared effect, with the effect-vocabulary diagnostic" in {
    account("def main: Unit = printLine(\"hello\")")
      .asserting(result =>
        (
          result._1.map(_.message).exists(_.contains("performs the effect 'Console' but does not declare it")),
          result._2.isDefined
        ) shouldBe (true, false)
      )
  }

  it should "reject an undeclared effect that propagates from a callee" in {
    account("def greet: {Console} Unit = printLine(\"hi\")\ndef main: Unit = greet")
      .asserting(result =>
        (
          result._1.map(_.message).exists(_.contains("performs the effect 'Console'")),
          result._2.isDefined
        ) shouldBe (true, false)
      )
  }

  private def account(source: String, name: String = "main"): IO[(Seq[TestError], Option[EffectAccounting])] = {
    val key = EffectAccounting.Key(ValueFQN(testModuleName, default(name)), Seq.empty)
    runGenerator(source, key, systemImports).map { case (errors, facts) =>
      (toTestErrors(errors), facts.get(key).collect { case ea: EffectAccounting => ea })
    }
  }
}
