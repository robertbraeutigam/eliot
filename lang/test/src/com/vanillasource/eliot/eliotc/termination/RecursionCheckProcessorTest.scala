package com.vanillasource.eliot.eliotc.termination

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.namedvalues.NamedValues
import com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesIndex
import com.vanillasource.eliot.eliotc.namedvalues.processor.NamedValuesRewriteProcessor
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{FunctionApplication, StringLiteral, ValueReference}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.resolve.fact.{Qualifier => RQualifier, QualifiedName => RQualifiedName}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue
import com.vanillasource.eliot.eliotc.termination.processor.RecursionCheckProcessor

/** Recursion-gate tests focused on the `namedValues` reflection edge. The gate reads the *rewritten* value (its
  * `namedValues` calls already expanded into `ref(...)` edges), so a cycle that closes through reflection must be
  * rejected exactly like a hand-written one — including a mutual cycle whose back-path runs through a *second*
  * reflection, which slipped the gate while transitive callees were read from the pre-rewrite fact.
  */
class RecursionCheckProcessorTest extends ProcessorTest(NamedValuesRewriteProcessor(), RecursionCheckProcessor()) {
  private val typeArg: Sourced[OperatorResolvedExpression] = sourced(ValueReference(sourced(WellKnownTypes.typeFQN)))

  private def vfqn(module: String, name: String): ValueFQN =
    ValueFQN(ModuleName(Seq("pkg"), module), QualifiedName(name, Qualifier.Default))

  private def ref(fqn: ValueFQN): OperatorResolvedExpression = ValueReference(sourced(fqn))

  /** `namedValues[Type](name)`. */
  private def namedValuesCall(name: String): OperatorResolvedExpression =
    FunctionApplication(
      sourced(ValueReference(sourced(NamedValues.namedValuesFQN), Seq(typeArg))),
      sourced(StringLiteral(sourced(name)))
    )

  /** An operator-resolved value `vfqn` named `name` with the given runtime body. */
  private def value(vfqn: ValueFQN, name: String, body: OperatorResolvedExpression): OperatorResolvedValue =
    OperatorResolvedValue(
      vfqn,
      sourced(RQualifiedName(name, RQualifier.Default)),
      Some(sourced(body)),
      sourced(ref(WellKnownTypes.typeFQN))
    )

  private def check(target: ValueFQN, facts: CompilerFact*): IO[(Option[RecursionCheckedValue], Seq[String])] =
    runGeneratorWithFacts(facts, RecursionCheckedValue.Key(target, Platform.Runtime))
      .map { case (result, errors) => (result, errors.map(_.message)) }

  private val fFqn = vfqn("F", "foo")
  private val gFqn = vfqn("G", "bar")

  "the recursion gate" should "reject a value that reflects itself directly" in {
    check(
      fFqn,
      NamedValuesIndex("foo", Platform.Runtime, Seq(fFqn)),
      value(fFqn, "foo", namedValuesCall("foo"))
    ).asserting { case (result, errors) =>
      result shouldBe None
      errors shouldBe Seq("Value 'foo' is defined recursively.")
    }
  }

  it should "reject a mutual cycle whose back-path runs through a second reflection" in {
    // `foo = namedValues("bar")` and `bar = namedValues("foo")`: the runtime cycle foo -> bar -> foo closes through two
    // reflections. The transitive `bar` node must be read rewritten, else its `namedValues` is a body-less dead-end and
    // the cycle slips the gate.
    check(
      fFqn,
      NamedValuesIndex("bar", Platform.Runtime, Seq(gFqn)),
      NamedValuesIndex("foo", Platform.Runtime, Seq(fFqn)),
      value(fFqn, "foo", namedValuesCall("bar")),
      value(gFqn, "bar", namedValuesCall("foo"))
    ).asserting { case (result, errors) =>
      result shouldBe None
      errors shouldBe Seq("Value 'foo' is defined recursively.")
    }
  }

  it should "accept a reflection that does not cycle back" in {
    // `foo = namedValues("bar")`, and `bar` reflects nothing — no path back to `foo`.
    check(
      fFqn,
      NamedValuesIndex("bar", Platform.Runtime, Seq(gFqn)),
      value(fFqn, "foo", namedValuesCall("bar")),
      value(gFqn, "bar", ref(WellKnownTypes.typeFQN))
    ).asserting { case (result, errors) =>
      result.map(_.value.vfqn) shouldBe Some(fFqn)
      errors shouldBe Seq.empty
    }
  }
}
