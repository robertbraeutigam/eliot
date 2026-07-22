package com.vanillasource.eliot.eliotc.monomorphize.check

import com.vanillasource.eliot.eliotc.ast.fact.EffectRow
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.ResolvedAbilityConstraint
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pure core of the effects-as-channel Phase 2 shadow accounting: reading a value's declared effect row straight
  * from the channel metadata ([[EffectRow]], populated in Phase 1) rather than its carrier-binder constraints. The
  * full byte-identical-verdict equivalence against the current `EffectResidualChecker` is verified by the suite sweep
  * (docs/effects-as-channel.md §10 Phase 2); this pins the extraction rules the sweep relies on.
  */
class EffectResidualCheckerTest extends AnyFlatSpec with Matchers {

  private def ability(name: String): AbilityFQN =
    AbilityFQN(ModuleName(Seq("eliot", "effect"), name), name)

  private def entry(name: String): ResolvedAbilityConstraint = ResolvedAbilityConstraint(ability(name), Seq.empty)

  "the channel declared-row extraction" should "union the return and parameter positions" in {
    val row = EffectRow(Seq(entry("Console")), Seq(EffectRow.ParameterEffects(0, Seq(entry("Log")))))
    EffectResidualChecker.channelDeclaredEffects(row) shouldBe Set(ability("Console"), ability("Log"))
  }

  it should "drop the Effect and Suspend machinery abilities" in {
    val row = EffectRow(Seq(entry("Console"), entry("Effect")), Seq(EffectRow.ParameterEffects(0, Seq(entry("Suspend")))))
    EffectResidualChecker.channelDeclaredEffects(row) shouldBe Set(ability("Console"))
  }

  it should "be empty for a row carrying no open effects" in {
    EffectResidualChecker.channelDeclaredEffects(EffectRow.empty[ResolvedAbilityConstraint]) shouldBe Set.empty
  }
}
