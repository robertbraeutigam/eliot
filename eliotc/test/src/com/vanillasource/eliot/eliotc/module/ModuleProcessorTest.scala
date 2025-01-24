package com.vanillasource.eliot.eliotc.module

import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.token.Tokenizer

class ModuleProcessorTest extends ProcessorTest(Tokenizer(), ASTParser(), ModuleProcessor()) {
  "module processor" should "issue no errors on valid function" in {
    runEngineForErrors("a: A").asserting(_ shouldBe Seq())
  }

  it should "detect duplicate functions" in {
    runEngineForErrors("a: A\na: A").asserting(_ shouldBe Seq("Function was already defined in this module."))
  }

  it should "fail if parameter names are not unique" in {
    runEngineForErrors("a(b: B, b: B): A").asserting(_ shouldBe Seq("Duplicate parameter names."))
  }
}
