package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors

/** Effects-as-channel Phase 3 (docs/effects-as-channel.md §10) — the *foundation* checkpoint of the gated new path.
  * Under `--effect-channel` the desugar strips open effect rows to their payload and erases the carrier from effect
  * abilities, so the checker is **effect-blind**: an effectful program monomorphizes with its effect operations left as
  * abstract ability-method references (which the later weaver slice resolves at an assigned carrier). This suite pins
  * that: an effectful `main` produces a [[MonomorphicValue]] with no errors, whereas a first-order ability with no
  * instance still errors — the effect-blindness is scoped to carrier-parametric (effect) abilities only.
  *
  * The default (flag-off) carrier path is exercised, unchanged, by the rest of the monomorphize suites.
  */
class EffectChannelDesugarTest extends ProcessorTest(LangProcessors(effectChannel = true)*) {

  "the effect-channel desugar" should "monomorphize a Suspend-riding (Console) program effect-blind" in {
    monoOf("def main: {Console} Unit = printLine(\"hello\")")
      .asserting(result => (result._1, result._2.isDefined) shouldBe (Seq.empty, true))
  }

  it should "monomorphize a control-effect (Abort) program effect-blind" in {
    monoOf("def f: {Abort} String = abort", name = "f")
      .asserting(result => (result._1, result._2.isDefined) shouldBe (Seq.empty, true))
  }

  it should "leave the effect operation as an unresolved ability-method reference for the weaver" in {
    monoOf("def main: {Console} Unit = printLine(\"hello\")")
      .asserting(result => valueReferences(result._2).map(_.name.qualifier) should contain(Qualifier.Ability("Console")))
  }

  it should "still error on a first-order ability with no instance (blindness is scoped to effect abilities)" in {
    monoOf("def f(x: String): String = show(x)", name = "f")
      .asserting(_._1.exists(_.message.contains("Show")) shouldBe true)
  }

  private def monoOf(source: String, name: String = "main"): IO[(Seq[TestError], Option[MonomorphicValue])] = {
    val key = MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), Seq.empty)
    runGenerator(source, key, systemImports).map { case (errors, facts) =>
      (toTestErrors(errors), facts.get(key).collect { case mv: MonomorphicValue => mv })
    }
  }

  /** Every value reference in a monomorphic value's body, in traversal order. */
  private def valueReferences(mv: Option[MonomorphicValue]): Seq[ValueFQN] =
    mv.flatMap(_.runtime).toSeq.flatMap(body => collect(body.value))

  private def collect(expr: MonomorphicExpression.Expression): Seq[ValueFQN] = expr match {
    case MonomorphicExpression.MonomorphicValueReference(vfqn, _) => Seq(vfqn.value)
    case MonomorphicExpression.FunctionApplication(target, arg)  =>
      collect(target.value.expression) ++ collect(arg.value.expression)
    case MonomorphicExpression.FunctionLiteral(_, _, body)       => collect(body.value.expression)
    case _                                                       => Seq.empty
  }
}
