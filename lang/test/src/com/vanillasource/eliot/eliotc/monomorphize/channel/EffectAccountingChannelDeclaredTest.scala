package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.ast.fact.EffectRow
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityConstraint, AbilityFQN}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pure core of the effect accounting's **rendering-side** channel-declared extraction
  * ([[EffectAccountingProcessor.channelDeclaredEffects]]): reading a value's declared effect row straight from the
  * channel metadata ([[EffectRow]], populated in Phase 1) for the LSP's declared-row vocabulary (§4/§5). This is **not**
  * the verification input — since U4-c-0b, `derived ⊆ declared` reads "declared" from the carrier-binder constraints
  * ([[EffectAccountingProcessor.declaredEffectsOf]], the single source of truth); this pins only the row-rendering
  * rules. (Successor to the Phase-2 `EffectResidualChecker.channelDeclaredEffects` unit test.)
  */
class EffectAccountingChannelDeclaredTest extends AnyFlatSpec with Matchers {

  private def ability(name: String): AbilityFQN =
    AbilityFQN(ModuleName(Seq("eliot", "effect"), name), name)

  private def entry(name: String): AbilityConstraint[OperatorResolvedExpression] =
    AbilityConstraint(ability(name), Seq.empty)

  "the channel declared-row rendering extraction" should "union the return and parameter positions" in {
    val row = EffectRow(Seq(entry("Console")), Seq(EffectRow.ParameterEffects(0, Seq(entry("Log")))))
    EffectAccountingProcessor.channelDeclaredEffects(row) shouldBe Set(ability("Console"), ability("Log"))
  }

  it should "drop the Effect and Suspend machinery abilities" in {
    val row = EffectRow(Seq(entry("Console"), entry("Effect")), Seq(EffectRow.ParameterEffects(0, Seq(entry("Suspend")))))
    EffectAccountingProcessor.channelDeclaredEffects(row) shouldBe Set(ability("Console"))
  }

  it should "be empty for a row carrying no open effects" in {
    EffectAccountingProcessor.channelDeclaredEffects(
      EffectRow.empty[AbilityConstraint[OperatorResolvedExpression]]
    ) shouldBe Set.empty
  }
}
