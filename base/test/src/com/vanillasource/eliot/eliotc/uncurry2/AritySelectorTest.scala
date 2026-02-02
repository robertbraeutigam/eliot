package com.vanillasource.eliot.eliotc.uncurry2

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.used2.UsedNames.UsageStats
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AritySelectorTest extends AnyFlatSpec with Matchers {

  private def functionType(paramType: Value, returnType: Value): Value =
    Value.Structure(
      Map(
        "$typeName" -> Value.Direct(Types.functionDataTypeFQN, Types.fullyQualifiedNameType),
        "A"         -> paramType,
        "B"         -> returnType
      ),
      Value.Type
    )

  private val intType    = Types.bigIntType
  private val stringType = Types.stringType

  "computeMaxArity" should "return 0 for non-function type" in {
    AritySelector.computeMaxArity(intType) shouldBe 0
  }

  it should "return 1 for A -> B" in {
    val sig = functionType(intType, stringType)
    AritySelector.computeMaxArity(sig) shouldBe 1
  }

  it should "return 2 for A -> B -> C" in {
    val sig = functionType(intType, functionType(intType, stringType))
    AritySelector.computeMaxArity(sig) shouldBe 2
  }

  it should "return 3 for A -> B -> C -> D" in {
    val sig = functionType(intType, functionType(intType, functionType(intType, stringType)))
    AritySelector.computeMaxArity(sig) shouldBe 3
  }

  "selectOptimalArity" should "return max arity when no stats are available" in {
    val sig = functionType(intType, functionType(intType, stringType))
    AritySelector.selectOptimalArity(None, sig) shouldBe 2
  }

  it should "return max arity when stats are empty" in {
    val sig   = functionType(intType, functionType(intType, stringType))
    val stats = UsageStats(Seq.empty, Map.empty)
    AritySelector.selectOptimalArity(Some(stats), sig) shouldBe 2
  }

  it should "return the arity with highest frequency" in {
    val sig   = functionType(intType, functionType(intType, functionType(intType, stringType)))
    val stats = UsageStats(Seq.empty, Map(0 -> 5, 1 -> 3, 2 -> 10, 3 -> 2))
    AritySelector.selectOptimalArity(Some(stats), sig) shouldBe 2
  }

  it should "prefer higher arity when frequencies are equal" in {
    val sig   = functionType(intType, functionType(intType, stringType))
    val stats = UsageStats(Seq.empty, Map(0 -> 1, 1 -> 1, 2 -> 1))
    AritySelector.selectOptimalArity(Some(stats), sig) shouldBe 2
  }

  it should "return 0 when function is used mostly as value" in {
    val sig   = functionType(intType, stringType)
    val stats = UsageStats(Seq.empty, Map(0 -> 100, 1 -> 1))
    AritySelector.selectOptimalArity(Some(stats), sig) shouldBe 0
  }

  it should "not exceed max arity even when stats suggest higher" in {
    val sig   = functionType(intType, stringType) // max arity 1
    val stats = UsageStats(Seq.empty, Map(0 -> 1, 1 -> 5, 2 -> 10, 3 -> 20))
    AritySelector.selectOptimalArity(Some(stats), sig) shouldBe 1
  }

  it should "handle non-function types with stats" in {
    val stats = UsageStats(Seq.empty, Map(0 -> 5))
    AritySelector.selectOptimalArity(Some(stats), intType) shouldBe 0
  }

  "FunctionType extractor" should "match function types" in {
    val sig = functionType(intType, stringType)
    sig match {
      case AritySelector.FunctionType(param, ret) =>
        (param, ret) shouldBe (intType, stringType)
      case _                                      =>
        fail("Should have matched FunctionType")
    }
  }

  it should "not match non-function types" in {
    intType match {
      case AritySelector.FunctionType(_, _) =>
        fail("Should not have matched FunctionType")
      case _                                =>
        succeed
    }
  }
}
