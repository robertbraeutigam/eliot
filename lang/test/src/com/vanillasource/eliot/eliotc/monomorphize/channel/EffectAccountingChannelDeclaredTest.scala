package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.ast.fact.EffectRow
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.ResolvedAbilityConstraint
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pure core of the effect accounting's channel-declared computation
  * ([[EffectAccountingProcessor.channelDeclaredEffects]]): reading a value's declared effect row straight from the
  * channel metadata ([[EffectRow]], populated in Phase 1) rather than its carrier-binder constraints. This pins the
  * extraction rules the `derived ⊆ declared` verification relies on. (Successor to the Phase-2
  * `EffectResidualChecker.channelDeclaredEffects` unit test, retargeted when that in-checker shadow was removed.)
  */
class EffectAccountingChannelDeclaredTest extends AnyFlatSpec with Matchers {

  private def ability(name: String): AbilityFQN =
    AbilityFQN(ModuleName(Seq("eliot", "effect"), name), name)

  private def entry(name: String): ResolvedAbilityConstraint = ResolvedAbilityConstraint(ability(name), Seq.empty)

  "the channel declared-row extraction" should "union the return and parameter positions" in {
    val row = EffectRow(Seq(entry("Console")), Seq(EffectRow.ParameterEffects(0, Seq(entry("Log")))))
    EffectAccountingProcessor.channelDeclaredEffects(row) shouldBe Set(ability("Console"), ability("Log"))
  }

  it should "drop the Effect and Suspend machinery abilities" in {
    val row = EffectRow(Seq(entry("Console"), entry("Effect")), Seq(EffectRow.ParameterEffects(0, Seq(entry("Suspend")))))
    EffectAccountingProcessor.channelDeclaredEffects(row) shouldBe Set(ability("Console"))
  }

  it should "be empty for a row carrying no open effects" in {
    EffectAccountingProcessor.channelDeclaredEffects(EffectRow.empty[ResolvedAbilityConstraint]) shouldBe Set.empty
  }
}
