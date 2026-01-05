package com.vanillasource.eliot.eliotc.module

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleFunction}
import com.vanillasource.eliot.eliotc.module.processor.{ModuleDataProcessor, ModuleFunctionProcessor}
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.sugar.DesugarProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer

class ModuleProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      DesugarProcessor(),
      SequentialCompilerProcessors(Seq(ModuleFunctionProcessor(Seq.empty), ModuleDataProcessor(Seq.empty)))
    ) {
  "module processor" should "issue no errors on valid function" in {
    runEngineForErrors("a: A").asserting(_ shouldBe Seq())
  }

  it should "detect duplicate functions" in {
    runEngineForErrors("a: A\na: A").asserting(_ shouldBe Seq("Function was already defined in this module."))
  }

  it should "fail if parameter names are not unique" in {
    runEngineForErrors("a(b: B, b: B): A").asserting(_ shouldBe Seq("Duplicate parameter name."))
  }

  it should "issue error if import can not be found" in {
    runEngineForErrors("import A").asserting(_ shouldBe Seq("Could not find imported module."))
  }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(source, ModuleFunction.Key(file, FunctionFQN(testModuleName, "a"))).map(_._1.map(_.message))
}
