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

  it should "carry the leading auto-marked binder count of a type constructor" in {
    runEngineForType("type Int[auto MIN: BigInteger, auto MAX: BigInteger]", "Int")
      .asserting(_.namedValue.inferableArity shouldBe 2)
  }

  it should "carry a zero count for an unmarked type parameter" in {
    runEngineForType("type IO[A]", "IO").asserting(_.namedValue.inferableArity shouldBe 0)
  }

  it should "count only the leading run of auto-marked parameters" in {
    runEngineForType("type Pair[auto A, B]", "Pair").asserting(_.namedValue.inferableArity shouldBe 1)
  }

  it should "not count an auto-marked parameter that follows an unmarked one" in {
    runEngineForType("type Pair[A, auto B]", "Pair").asserting(_.namedValue.inferableArity shouldBe 0)
  }

  it should "carry the count of auto-marked generic parameters of a function" in {
    runEngineForValue("def f[auto A, B]: A", "f").asserting(_.namedValue.inferableArity shouldBe 1)
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
