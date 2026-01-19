package com.vanillasource.eliot.eliotc.module2.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName => ModuleName2, UnifiedModuleValue, ValueFQN}
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
    runEngineForValue("a: A", "a").asserting(_.vfqn shouldBe ValueFQN(testModuleName2, "a"))
  }

  it should "prefer implemented value over abstract" in {
    runEngineForValue("a: A = b", "a").asserting(_.namedValue.value.hasRuntime shouldBe true)
  }

  it should "include dictionary from source module value" in {
    runEngineForValue("a: A\nb: B", "a").asserting { uv =>
      uv.dictionary shouldBe Map(
        "a" -> ValueFQN(testModuleName2, "a"),
        "b" -> ValueFQN(testModuleName2, "b")
      )
    }
  }

  it should "abort if value not found" in {
    runEngineForErrors("a: A", "nonexistent").asserting(_ shouldBe Seq.empty)
  }

  private def runEngineForValue(source: String, name: String): IO[UnifiedModuleValue] =
    runGenerator(source, UnifiedModuleValue.Key(ValueFQN(testModuleName2, name))).map { case (_, facts) =>
      facts.values.collectFirst { case uv: UnifiedModuleValue if uv.vfqn.name == name => uv }.get
    }

  private def runEngineForErrors(source: String, name: String): IO[Seq[String]] =
    runGenerator(source, UnifiedModuleValue.Key(ValueFQN(testModuleName2, name))).map(_._1.map(_.message))
}
