package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors

/** Effects-as-channel Phase 3 §6 — the **weaver**, first slice ([[WovenValueProcessor]]): under `--effect-channel` with
  * a platform-supplied base carrier, the effect-blind `MonomorphicValue`'s abstract user effect operations are resolved
  * to their concrete carrier-instance methods, and an effectful value's signature is carrier-wrapped. This suite drives
  * the weave against a self-contained fake effect ability `Foo[F[_]]` and a concrete carrier `TestIO`, so the weaver's
  * carrier-assignment + effect-operation resolution is exercised without the full jvm `IO` machinery: the abstract
  * `Foo::op` (`Qualifier.Ability`) must become a `Qualifier.AbilityImplementation` reference (the resolved instance
  * method), and the woven signature must be `TestIO`-headed.
  */
class WovenValueTest
    extends ProcessorTest(
      LangProcessors(
        effectChannel = true,
        baseCarrier = Some(ValueFQN(ModuleName(Seq.empty, "Test"), QualifiedName("TestIO", Qualifier.Type)))
      )*
    ) {

  private val prelude =
    """type TestIO[A]
      |ability Foo[F[_]] { def op: F[Unit] }
      |implement Foo[TestIO] { def op: TestIO[Unit] }
      |""".stripMargin

  "the weaver" should "resolve an abstract user effect operation to the concrete carrier instance method" in {
    woven(prelude + "def main: {Foo} Unit = op")
      .asserting(result => result._2.exists(mv => qualifiers(mv).exists {
        case Qualifier.AbilityImplementation("Foo", _) => true
        case _                                         => false
      }) shouldBe true)
  }

  it should "leave no abstract Ability reference to the effect operation in the woven body" in {
    woven(prelude + "def main: {Foo} Unit = op")
      .asserting(result => result._2.exists(mv => qualifiers(mv).contains(Qualifier.Ability("Foo"))) shouldBe false)
  }

  it should "wrap an effectful value's signature in the base carrier" in {
    woven(prelude + "def main: {Foo} Unit = op")
      .asserting(result => result._2.map(_.signature).collect {
        case GroundValue.Structure(fqn, _, _) => fqn.name.name
      } shouldBe Some("TestIO"))
  }

  it should "produce no errors weaving a well-formed effectful program" in {
    woven(prelude + "def main: {Foo} Unit = op").asserting(_._1 shouldBe Seq.empty)
  }

  private def woven(source: String, name: String = "main"): IO[(Seq[TestError], Option[WovenValue])] = {
    val key = WovenValue.Key(ValueFQN(testModuleName, default(name)), Seq.empty)
    runGenerator(source, key, systemImports).map { case (errors, facts) =>
      (toTestErrors(errors), facts.get(key).collect { case wv: WovenValue => wv })
    }
  }

  private def qualifiers(wv: WovenValue): Seq[Qualifier] =
    wv.runtime.toSeq.flatMap(body => collect(body.value)).map(_.name.qualifier)

  private def collect(expr: MonomorphicExpression.Expression): Seq[ValueFQN] = expr match {
    case MonomorphicExpression.MonomorphicValueReference(vfqn, _) => Seq(vfqn.value)
    case MonomorphicExpression.FunctionApplication(target, arg)  =>
      collect(target.value.expression) ++ collect(arg.value.expression)
    case MonomorphicExpression.FunctionLiteral(_, _, body)       => collect(body.value.expression)
    case _                                                       => Seq.empty
  }
}
