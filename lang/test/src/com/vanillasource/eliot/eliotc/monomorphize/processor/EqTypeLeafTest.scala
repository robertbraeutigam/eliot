package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{stringFQN, typeEqualsFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{BindingContribution, ContributedBinding, GroundValue}

/** Ability guards Stage 0: the compiler-pool `Eq[Type]` structural-equality leaf — `typeEquals(a: Type, b: Type): Bool`,
  * whose reduction is supplied by [[SystemNativesProcessor]].
  *
  * `Eq[Type]` is compiler-pool-only (types are erased at runtime) and only fully reduces in a type/signature position —
  * which end-to-end means a compile-time guard (`where E1 != E2`), landing in later stages of the plan. So Stage 0
  * verifies the leaf itself directly: pull the native `SemValue` out of the `system` [[ContributedBinding]] and apply it
  * to type normal forms via [[Evaluator.applyValue]], asserting it reduces two equal types to `true`, two distinct types
  * to `false`, and stays stuck (fail-safe) on a non-concrete argument. Types are `VTopDef(head, None, spine)` normal
  * forms — exactly what the checker hands a guard at a ground use site — so `Int` is `VTopDef(IntFQN)` and `Int[0,5]` is
  * `VTopDef(IntFQN, …, [0, 5])`.
  */
class EqTypeLeafTest extends ProcessorTest(SystemNativesProcessor()) {

  private val intFQN: ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "lang"), "Int"), QualifiedName("Int", Qualifier.Type))

  private def typeCtor(fqn: ValueFQN, args: SemValue*): SemValue =
    VTopDef(fqn, None, args.foldLeft(Spine.SNil: Spine)(_ :+ _))

  private def bound(n: Int): SemValue = VConst(GroundValue.Direct(BigInt(n), Evaluator.bigIntGroundType))

  private val intType: SemValue    = typeCtor(intFQN)
  private val stringType: SemValue = typeCtor(stringFQN)

  /** The `typeEquals` native `SemValue`, pulled out of the `system` [[ContributedBinding]] `SystemNativesProcessor`
    * publishes for [[typeEqualsFQN]].
    */
  private val typeEqualsNative: IO[SemValue] =
    runGeneratorWithFacts(Seq.empty, ContributedBinding.Key(typeEqualsFQN, ContributedBinding.systemLabel)).map {
      case (Some(cb), _) =>
        cb.contributed match {
          case Some(BindingContribution.Leaf(sem)) => sem
          case other                               => fail(s"Expected a Leaf contribution, got: $other")
        }
      case (None, errors) => fail(s"No typeEquals binding produced. Errors: ${toTestErrors(errors)}")
    }

  private def apply2(a: SemValue, b: SemValue): IO[SemValue] =
    typeEqualsNative.map(native => Evaluator.applyValue(Evaluator.applyValue(native, a), b))

  "the Eq[Type] leaf" should "reduce two identical bare types to true (Int == Int)" in {
    apply2(intType, intType).asserting(_ shouldBe Evaluator.trueValue)
  }

  it should "reduce two distinct bare types to false (Int == String)" in {
    apply2(intType, stringType).asserting(_ shouldBe Evaluator.falseValue)
  }

  it should "compare bounded types structurally: Int[0, 5] == Int[0, 5] is true" in {
    apply2(typeCtor(intFQN, bound(0), bound(5)), typeCtor(intFQN, bound(0), bound(5)))
      .asserting(_ shouldBe Evaluator.trueValue)
  }

  it should "distinguish differing bounds: Int[0, 5] == Int[0, 10] is false" in {
    apply2(typeCtor(intFQN, bound(0), bound(5)), typeCtor(intFQN, bound(0), bound(10)))
      .asserting(_ shouldBe Evaluator.falseValue)
  }

  it should "stay stuck (fail-safe) on a non-concrete argument rather than answer wrongly" in {
    apply2(VMeta(MetaId(0), Spine.SNil), intType).asserting(_ shouldBe VStuckNative(typeEqualsFQN, Spine.SNil :+ VMeta(MetaId(0), Spine.SNil) :+ intType))
  }

  it should "stay stuck when a bound is a non-concrete metavariable" in {
    apply2(typeCtor(intFQN, bound(0), VMeta(MetaId(1), Spine.SNil)), typeCtor(intFQN, bound(0), bound(5)))
      .asserting(_ shouldBe a[VStuckNative])
  }
}
