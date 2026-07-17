package com.vanillasource.eliot.eliotc.namedvalues

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.namedvalues.fact.{NamedValuesIndex, NamedValuesRewrittenValue}
import com.vanillasource.eliot.eliotc.namedvalues.processor.NamedValuesRewriteProcessor
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{FunctionApplication, StringLiteral, ValueReference}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.resolve.fact.{Qualifier => RQualifier, QualifiedName => RQualifiedName}
import com.vanillasource.eliot.eliotc.source.content.Sourced

class NamedValuesRewriteProcessorTest extends ProcessorTest(NamedValuesRewriteProcessor()) {
  private val mainVfqn = ValueFQN(ModuleName(Seq.empty, "Test"), QualifiedName("main", Qualifier.Default))
  private val testType = ValueFQN(ModuleName(Seq("pkg"), "Test"), QualifiedName("Test", Qualifier.Type))
  private val specA    = ValueFQN(ModuleName(Seq("pkg"), "A"), QualifiedName("spec", Qualifier.Default))
  private val specB    = ValueFQN(ModuleName(Seq("pkg"), "B"), QualifiedName("spec", Qualifier.Default))

  private val typeArg: Sourced[OperatorResolvedExpression] = sourced(ValueReference(sourced(testType)))

  private def ref(fqn: ValueFQN, typeArgs: Seq[Sourced[OperatorResolvedExpression]] = Seq.empty): OperatorResolvedExpression =
    ValueReference(sourced(fqn), typeArgs)

  /** `namedValues[Test](nameArg)`. */
  private def namedValuesCall(nameArg: OperatorResolvedExpression): OperatorResolvedExpression =
    FunctionApplication(sourced(ref(NamedValues.namedValuesFQN, Seq(typeArg))), sourced(nameArg))

  /** The chain the rewrite is expected to emit: `prepend[Test](fqn₁, prepend[Test](fqn₂, … empty[Test]))`. */
  private def expectedChain(fqns: Seq[ValueFQN]): OperatorResolvedExpression =
    fqns.foldRight(ref(NamedValues.listEmptyFQN, Seq(typeArg))) { (fqn, tail) =>
      FunctionApplication(
        sourced(FunctionApplication(sourced(ref(NamedValues.listPrependFQN, Seq(typeArg))), sourced(ref(fqn)))),
        sourced(tail)
      )
    }

  private def mainValue(body: OperatorResolvedExpression): OperatorResolvedValue =
    OperatorResolvedValue(
      mainVfqn,
      sourced(RQualifiedName("main", RQualifier.Default)),
      Some(sourced(body)),
      sourced(ref(WellKnownTypes.typeFQN))
    )

  private def rewrite(facts: CompilerFact*): IO[(Option[NamedValuesRewrittenValue], Seq[TestError])] =
    runGeneratorWithFacts(facts, NamedValuesRewrittenValue.Key(mainVfqn, Platform.Runtime))
      .map { case (result, errors) => (result, toTestErrors(errors)) }

  private def rewrittenBody(facts: CompilerFact*): IO[OperatorResolvedExpression] =
    rewrite(facts*).map(_._1.get.value.runtime.get.value)

  "named values rewrite" should "expand a literal call into the prepend/empty chain over the index entries" in {
    rewrittenBody(
      NamedValuesIndex("spec", Platform.Runtime, Seq(specA, specB)),
      mainValue(namedValuesCall(StringLiteral(sourced("spec"))))
    ).asserting(_ shouldBe expectedChain(Seq(specA, specB)))
  }

  it should "expand an empty index into just the empty builder" in {
    rewrittenBody(
      NamedValuesIndex("spec", Platform.Runtime, Seq.empty),
      mainValue(namedValuesCall(StringLiteral(sourced("spec"))))
    ).asserting(_ shouldBe expectedChain(Seq.empty))
  }

  it should "rewrite a call nested inside another application" in {
    rewrittenBody(
      NamedValuesIndex("spec", Platform.Runtime, Seq(specA)),
      mainValue(FunctionApplication(sourced(ref(specB)), sourced(namedValuesCall(StringLiteral(sourced("spec"))))))
    ).asserting(_ shouldBe FunctionApplication(sourced(ref(specB)), sourced(expectedChain(Seq(specA)))))
  }

  it should "leave a body that uses no reflection unchanged" in {
    rewrittenBody(mainValue(ref(specA))).asserting(_ shouldBe ref(specA))
  }

  it should "hard-error on a non-literal name argument" in {
    rewrite(mainValue(namedValuesCall(ref(specA)))).asserting { case (result, errors) =>
      result shouldBe None
      errors.map(_.message) shouldBe Seq("The 'namedValues' reflection primitive requires a literal String name.")
    }
  }

  it should "hard-error on a bare unapplied reference" in {
    rewrite(mainValue(ref(NamedValues.namedValuesFQN, Seq(typeArg)))).asserting { case (result, errors) =>
      result shouldBe None
      errors.map(_.message) shouldBe
        Seq("The 'namedValues' reflection primitive must be applied directly to a literal String name.")
    }
  }
}
