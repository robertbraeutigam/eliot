package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, Role, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** The signature split, Step 1: every definition mints a `Signature` twin beside its `Runtime` twin, flowing through the
  * *same* value machinery ([[ModuleValue]] / [[UnifiedModuleValue]]) but staying out of the surface name set. This pins:
  *
  *   - the signature twin is a first-class, demandable [[UnifiedModuleValue]] fact keyed by its role-bearing FQN, and it
  *     is always bodied (its body is the signature expression);
  *   - the recast per-`(name, role)` merge: signature twins all *agree* rather than following prefer-the-implementation —
  *     so two co-located **concrete** layers, which are "Has multiple implementations." for the runtime twin, merge
  *     cleanly for the signature twin;
  *   - differing signatures across layers are still rejected;
  *   - the surface merge (the runtime twin) is byte-identical to before the split.
  */
class SignatureTwinMergeTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {
  private val moduleName = ModuleName(Seq.empty, "M")
  private val modulePath = Path.of("M.els")
  private val baseUri     = URI.create("baseM.els")
  private val jvmUri      = URI.create("jvmM.els")

  private def runtimeFqn   = ValueFQN(moduleName, QualifiedName("foo", Qualifier.Default))
  private def signatureFqn = ValueFQN(moduleName, QualifiedName("foo", Qualifier.Default, Role.Signature))

  private def source(uri: URI, content: String): SourceContent =
    SourceContent(uri, Sourced(uri, PositionRange.zero, content))

  private def unify(facts: Seq[CompilerFact], vfqn: ValueFQN): IO[(Option[UnifiedModuleValue], Seq[TestError])] =
    runGeneratorWithFacts(facts, UnifiedModuleValue.Key(vfqn, Platform.Runtime))
      .map { case (result, errors) => (result, toTestErrors(errors)) }

  private val singleAbstract = Seq(
    PathScan(modulePath, Seq(baseUri), Platform.Runtime),
    source(baseUri, "def foo: A")
  )

  // Base declares `foo` abstractly, the jvm layer implements it — the ordinary abstract/concrete split.
  private val abstractPlusConcrete = Seq(
    PathScan(modulePath, Seq(baseUri, jvmUri), Platform.Runtime),
    source(baseUri, "def foo: A"),
    source(jvmUri, "def foo: A = b")
  )

  // Two *concrete* co-located layers with the same signature.
  private val twoConcrete = Seq(
    PathScan(modulePath, Seq(baseUri, jvmUri), Platform.Runtime),
    source(baseUri, "def foo: A = b"),
    source(jvmUri, "def foo: A = b")
  )

  // Two layers that disagree on the signature.
  private val differingSignatures = Seq(
    PathScan(modulePath, Seq(baseUri, jvmUri), Platform.Runtime),
    source(baseUri, "def foo: A"),
    source(jvmUri, "def foo: B")
  )

  "the signature twin" should "be a demandable unified value fact carrying the Signature role" in {
    unify(singleAbstract, signatureFqn).asserting(_._1.map(_.vfqn.name.role) shouldBe Some(Role.Signature))
  }

  it should "always be bodied — its body is the signature expression — even when the runtime twin is abstract" in {
    unify(singleAbstract, signatureFqn).asserting(_._1.flatMap(_.namedValue.runtime).isDefined shouldBe true)
  }

  it should "stay out of the surface name set (the runtime twin's dictionary has no Signature-role entry)" in {
    unify(singleAbstract, runtimeFqn)
      .asserting(_._1.toSeq.flatMap(_.dictionary.keys).map(_.role) should not contain Role.Signature)
  }

  "the recast merge" should "let two concrete layers agree for the signature twin (no 'multiple implementations')" in {
    unify(twoConcrete, signatureFqn).asserting(_._2 shouldBe Seq.empty)
  }

  it should "still reject two concrete layers for the runtime twin" in {
    unify(twoConcrete, runtimeFqn).asserting(_._2.map(_.message) shouldBe Seq("Has multiple implementations."))
  }

  it should "merge an abstract-plus-concrete signature twin with no error" in {
    unify(abstractPlusConcrete, signatureFqn).asserting(_._2 shouldBe Seq.empty)
  }

  it should "reject layers whose signatures differ for the signature twin" in {
    unify(differingSignatures, signatureFqn).asserting(_._2.map(_.message) shouldBe Seq("Has multiple different definitions."))
  }
}
