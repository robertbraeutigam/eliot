package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName => ModuleName2, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.token.Tokenizer

class UnifiedModuleValueProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq.empty),
      UnifiedModuleValueProcessor()
    ) {
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
    runEngineForErrors("def a: A", "nonexistent").asserting(_ shouldBe Seq.empty)
  }

  private def runEngineForValue(source: String, name: String): IO[UnifiedModuleValue] =
    runGenerator(source, UnifiedModuleValue.Key(ValueFQN(testModuleName2, QualifiedName(name, Qualifier.Default))))
      .map { case (_, facts) =>
        facts.values.collectFirst {
          case uv: UnifiedModuleValue if uv.vfqn.name == QualifiedName(name, Qualifier.Default) => uv
        }.get
      }

  private def runEngineForErrors(source: String, name: String): IO[Seq[String]] =
    runGenerator(source, UnifiedModuleValue.Key(ValueFQN(testModuleName2, QualifiedName(name, Qualifier.Default))))
      .map(_._1.map(_.message))
}
