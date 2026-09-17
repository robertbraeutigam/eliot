package com.vanillasource.eliot.eliotc.progress

import com.vanillasource.eliot.eliotc.progress.ProgressPhase.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProgressHistoryTest extends AnyFlatSpec with Matchers {

  "a history" should "weigh the newest run as half of what it holds" in {
    ProgressHistory(10, Map(SavingCache -> 4), Map("parse" -> ProgressCost(10, 20)))
      .including(ProgressHistory(30, Map(SavingCache -> 8), Map("parse" -> ProgressCost(30, 40)))) shouldBe
      ProgressHistory(20, Map(SavingCache -> 6), Map("parse" -> ProgressCost(20, 30)))
  }

  it should "let a phase the newest run did not have fade" in {
    ProgressHistory(1, Map(LoadingCache -> 4), Map.empty).including(ProgressHistory(1, Map.empty, Map.empty)).phases shouldBe
      Map(LoadingCache -> 2)
  }

  it should "forget a key type once it averages less than half a fact" in {
    ProgressHistory(1, Map.empty, Map("gone" -> ProgressCost(0.8, 8), "kept" -> ProgressCost(1, 1)))
      .including(ProgressHistory(1, Map.empty, Map("kept" -> ProgressCost(1, 1))))
      .types shouldBe Map("kept" -> ProgressCost(1, 1))
  }

  it should "cost nothing on average for a key type it has no fact of" in {
    ProgressCost.zero.average shouldBe None
  }
}
