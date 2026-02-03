package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module.fact.ModuleNames
import com.vanillasource.eliot.eliotc.token.Tokenizer

class ModuleNamesProcessorTest extends ProcessorTest(Tokenizer(), ASTParser(), CoreProcessor(), ModuleNamesProcessor()) {

  "module names processor" should "extract a single name" in {
    runEngineForNames("a: A").asserting(_ shouldBe Set("a"))
  }

  it should "extract multiple names" in {
    runEngineForNames("a: A\nb: B").asserting(_ shouldBe Set("a", "b"))
  }

  it should "extract names from data definitions" in {
    runEngineForNames("data Person").asserting(_ shouldBe Set("Person$DataType"))
  }

  it should "extract constructor and accessor names from data with fields" in {
    runEngineForNames("data Person(name: Name)").asserting(_ shouldBe Set("Person$DataType", "Person", "name"))
  }

  it should "extract mixed function and data names" in {
    runEngineForNames("data A\nf: A").asserting(_ shouldBe Set("A$DataType", "f"))
  }

  it should "detect duplicate names" in {
    runEngineForErrors("a: A\na: B").asserting(_ shouldBe Seq("Name was already defined in this module."))
  }

  it should "return empty set for empty module" in {
    runEngineForNames("").asserting(_ shouldBe Set())
  }

  private def runEngineForNames(source: String): IO[Set[String]] =
    runGenerator(source, ModuleNames.Key(file)).map { case (_, facts) =>
      facts.values.collectFirst { case ModuleNames(_, names) => names }.getOrElse(Set.empty)
    }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(source, ModuleNames.Key(file)).map(_._1.map(_.message))
}
