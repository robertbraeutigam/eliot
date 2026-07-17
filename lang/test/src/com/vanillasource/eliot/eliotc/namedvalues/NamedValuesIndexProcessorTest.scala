package com.vanillasource.eliot.eliotc.namedvalues

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, Role, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesIndex
import com.vanillasource.eliot.eliotc.namedvalues.processor.NamedValuesIndexProcessor
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.source.scan.PoolModules

class NamedValuesIndexProcessorTest extends ProcessorTest(NamedValuesIndexProcessor()) {
  private val moduleA = ModuleName(Seq("pkg"), "A")
  private val moduleB = ModuleName(Seq("pkg"), "B")

  private def specFqn(module: ModuleName): ValueFQN = ValueFQN(module, QualifiedName("spec", Qualifier.Default))

  private def unified(module: ModuleName, names: (QualifiedName, Visibility)*): UnifiedModuleNames =
    UnifiedModuleNames(module, names.toMap, Platform.Runtime)

  private def publicDefault(name: String): (QualifiedName, Visibility) =
    QualifiedName(name, Qualifier.Default) -> Visibility.Public

  private def indexFor(facts: CompilerFact*): IO[Seq[ValueFQN]] =
    runGeneratorWithFacts(facts, NamedValuesIndex.Key("spec", Platform.Runtime)).map(_._1.get.fqns)

  "named values index" should "collect a public default 'spec' from every pool module, sorted by canonical FQN" in {
    indexFor(
      PoolModules(Platform.Runtime, Set(moduleB, moduleA)),
      unified(moduleA, publicDefault("spec")),
      unified(moduleB, publicDefault("spec"))
    ).asserting(_ shouldBe Seq(specFqn(moduleA), specFqn(moduleB)))
  }

  it should "keep distinct modules that both declare the name as distinct entries" in {
    indexFor(
      PoolModules(Platform.Runtime, Set(moduleA, moduleB)),
      unified(moduleA, publicDefault("spec")),
      unified(moduleB, publicDefault("spec"))
    ).asserting(_.toSet shouldBe Set(specFqn(moduleA), specFqn(moduleB)))
  }

  it should "exclude a private declaration of the name" in {
    indexFor(
      PoolModules(Platform.Runtime, Set(moduleA)),
      unified(moduleA, QualifiedName("spec", Qualifier.Default) -> Visibility.Private)
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "exclude a non-default-qualifier declaration of the name (e.g. a type named 'spec')" in {
    indexFor(
      PoolModules(Platform.Runtime, Set(moduleA)),
      unified(moduleA, QualifiedName("spec", Qualifier.Type) -> Visibility.Public)
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "exclude a signature-role twin of the name" in {
    indexFor(
      PoolModules(Platform.Runtime, Set(moduleA)),
      unified(moduleA, QualifiedName("spec", Qualifier.Default, Role.Signature) -> Visibility.Public)
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "ignore other names in the pool modules" in {
    indexFor(
      PoolModules(Platform.Runtime, Set(moduleA)),
      unified(moduleA, publicDefault("other"), publicDefault("spec"))
    ).asserting(_ shouldBe Seq(specFqn(moduleA)))
  }

  it should "be empty when no pool module declares the name" in {
    indexFor(
      PoolModules(Platform.Runtime, Set(moduleA, moduleB)),
      unified(moduleA, publicDefault("other")),
      unified(moduleB)
    ).asserting(_ shouldBe Seq.empty)
  }
}
