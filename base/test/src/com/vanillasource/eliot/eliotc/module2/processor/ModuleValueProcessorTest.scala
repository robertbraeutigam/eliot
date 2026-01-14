package com.vanillasource.eliot.eliotc.module2.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName => ModuleName2, ModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.token.Tokenizer

class ModuleValueProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq.empty)
    ) {
  private val testModuleName2 = ModuleName2(Seq.empty, "Test")

  "module value processor" should "create module value for a simple constant" in {
    runEngineForValue("a: A", "a").asserting { mv =>
      mv.vfqn shouldBe ValueFQN(testModuleName2, "a")
    }
  }

  it should "include local names in dictionary" in {
    runEngineForValue("a: A\nb: B", "a").asserting(_.dictionary.keySet shouldBe Set("a", "b"))
  }

  it should "map local names to correct FQNs in dictionary" in {
    runEngineForValue("a: A\nb: B", "a").asserting { mv =>
      mv.dictionary shouldBe Map(
        "a" -> ValueFQN(testModuleName2, "a"),
        "b" -> ValueFQN(testModuleName2, "b")
      )
    }
  }

  it should "detect duplicate names" in {
    runEngineForErrors("a: A\na: B").asserting(_ shouldBe Seq("Name was already defined in this module."))
  }

  it should "report error for missing imported module" in {
    runEngineForErrors("import a.b.C").asserting(_ shouldBe Seq("Could not find imported module."))
  }

  it should "include imported names in dictionary" in {
    val imp = SystemImport("Imported", "exported: A")
    runEngineForValue("import eliot.lang.Imported\na: A", "a", Seq(imp))
      .asserting(_.dictionary("exported") shouldBe ValueFQN(ModuleName2(Seq("eliot", "lang"), "Imported"), "exported"))
  }

  it should "detect imported names shadowing local names" in {
    val imp = SystemImport("Imported", "a: A")
    runEngineForErrors("import eliot.lang.Imported\na: A", Seq(imp))
      .asserting(_ shouldBe Seq("Imported names shadow local names: a"))
  }

  it should "detect imported names shadowing other imported names" in {
    val imp1 = SystemImport("Imported1", "shared: A")
    val imp2 = SystemImport("Imported2", "shared: B")
    runEngineForErrors("import eliot.lang.Imported1\nimport eliot.lang.Imported2\na: A", Seq(imp1, imp2))
      .asserting(errors => errors.head should include("Imported names shadow other imported names"))
  }

  private def runEngineForValue(
      source: String,
      name: String,
      imports: Seq[SystemImport] = Seq.empty
  ): IO[ModuleValue] =
    runGenerator(source, ModuleValue.Key(file, ValueFQN(testModuleName2, name)), imports).map { case (_, facts) =>
      facts.values.collectFirst { case mv: ModuleValue if mv.vfqn.name == name => mv }.get
    }

  private def runEngineForErrors(source: String, imports: Seq[SystemImport] = Seq.empty): IO[Seq[String]] =
    runGenerator(source, ModuleValue.Key(file, ValueFQN(testModuleName2, "a")), imports).map(_._1.map(_.message))
}
