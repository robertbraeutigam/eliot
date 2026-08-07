package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName => ModuleName2, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors

class UnifiedModuleValueProcessorTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {
  private val testModuleName2 = ModuleName2(Seq.empty, "Test")

  "unified module value processor" should "create unified value for a simple constant" in {
    runEngineForValue("def a: A", "a").asserting(
      _.vfqn shouldBe ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default))
    )
  }

  it should "prefer implemented value over abstract" in {
    runEngineForValue("def a: A = b", "a").asserting(_.namedValue.runtime.isDefined shouldBe true)
  }

  it should "include dictionary from source module value" in {
    runEngineForValue("def a: A\ndef b: B", "a").asserting { uv =>
      uv.dictionary shouldBe Map(
        QualifiedName("a", Qualifier.Default) -> ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default)),
        QualifiedName("b", Qualifier.Default) -> ValueFQN(testModuleName2, QualifiedName("b", Qualifier.Default))
      )
    }
  }

  it should "abort if value not found" in {
    runEngineForErrors("def a: A", "nonexistent").asserting(_ shouldBe Seq("Could not find 'nonexistent'." at ""))
  }

  // There is no user surface for inferable binders (the `auto` keyword was retired), so a user-written type or function
  // carries a zero inferable arity; the effect carrier — the one inferable binder — is synthesized in the core phase.
  it should "carry a zero inferable arity for a user-written type parameter" in {
    runEngineForType("type IO[A]", "IO").asserting(_.namedValue.inferableArity shouldBe 0)
  }

  it should "carry a zero inferable arity for a user-written function generic" in {
    runEngineForValue("def f[A, B]: A", "f").asserting(_.namedValue.inferableArity shouldBe 0)
  }

  private def runEngineForValue(source: String, name: String): IO[UnifiedModuleValue] =
    runEngineForName(source, QualifiedName(name, Qualifier.Default))

  private def runEngineForType(source: String, name: String): IO[UnifiedModuleValue] =
    runEngineForName(source, QualifiedName(name, Qualifier.Type))

  private def runEngineForName(source: String, name: QualifiedName): IO[UnifiedModuleValue] =
    runGenerator(source, UnifiedModuleValue.Key(ValueFQN(testModuleName2, name)))
      .map { case (_, facts) =>
        facts.values.collectFirst {
          case uv: UnifiedModuleValue if uv.vfqn.name == name => uv
        }.get
      }

  private def runEngineForErrors(source: String, name: String): IO[Seq[TestError]] =
    runGenerator(source, UnifiedModuleValue.Key(ValueFQN(testModuleName2, QualifiedName(name, Qualifier.Default))))
      .map(result => toTestErrors(result._1))
}
