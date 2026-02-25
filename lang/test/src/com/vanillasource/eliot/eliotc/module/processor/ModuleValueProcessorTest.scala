package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName => ModuleName2, ModuleValue, ValueFQN}
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
    runEngineForValue("def a: A", "a").asserting { mv =>
      mv.vfqn shouldBe ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default))
    }
  }

  it should "include local names in dictionary" in {
    runEngineForValue("def a: A\ndef b: B", "a").asserting(
      _.dictionary.keySet shouldBe Set(QualifiedName("a", Qualifier.Default), QualifiedName("b", Qualifier.Default))
    )
  }

  it should "map local names to correct FQNs in dictionary" in {
    runEngineForValue("def a: A\ndef b: B", "a").asserting { mv =>
      mv.dictionary shouldBe Map(
        QualifiedName("a", Qualifier.Default) -> ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default)),
        QualifiedName("b", Qualifier.Default) -> ValueFQN(testModuleName2, QualifiedName("b", Qualifier.Default))
      )
    }
  }

  it should "detect duplicate names" in {
    runEngineForErrors("def a: A\ndef a: B").asserting(_ shouldBe Seq("Name was already defined in this module." at "a"))
  }

  it should "report error for missing imported module" in {
    runEngineForErrors("import a.b.C").asserting(_ shouldBe Seq("Could not find imported module." at "a.b.C"))
  }

  it should "include imported names in dictionary" in {
    val imp = SystemImport("Imported", "def exported: A")
    runEngineForValue("import eliot.lang.Imported\ndef a: A", "a", Seq(imp))
      .asserting(
        _.dictionary(QualifiedName("exported", Qualifier.Default)) shouldBe ValueFQN(
          ModuleName2(Seq("eliot", "lang"), "Imported"),
          QualifiedName("exported", Qualifier.Default)
        )
      )
  }

  it should "detect imported names shadowing local names" in {
    val imp = SystemImport("Imported", "def a: A")
    runEngineForErrors("import eliot.lang.Imported\ndef a: A", Seq(imp))
      .asserting(_ shouldBe Seq("Imported names shadow local names: a" at "eliot.lang.Imported"))
  }

  it should "detect imported names shadowing other imported names" in {
    val imp1 = SystemImport("Imported1", "def shared: A")
    val imp2 = SystemImport("Imported2", "def shared: B")
    runEngineForErrors("import eliot.lang.Imported1\nimport eliot.lang.Imported2\ndef a: A", Seq(imp1, imp2))
      .asserting { errors =>
        errors.length shouldBe 1
        errors.head.message should include("Imported names shadow other imported names")
        errors.head.highlight shouldBe "eliot.lang.Imported2"
      }
  }

  private def runEngineForValue(
      source: String,
      name: String,
      imports: Seq[SystemImport] = Seq.empty
  ): IO[ModuleValue] =
    runGenerator(
      source,
      ModuleValue.Key(file, ValueFQN(testModuleName2, QualifiedName(name, Qualifier.Default))),
      imports
    ).map { case (_, facts) =>
      facts.values.collectFirst {
        case mv: ModuleValue if mv.vfqn.name == QualifiedName(name, Qualifier.Default) => mv
      }.get
    }

  private def runEngineForErrors(source: String, imports: Seq[SystemImport] = Seq.empty): IO[Seq[TestError]] =
    runGenerator(
      source,
      ModuleValue.Key(file, ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default))),
      imports
    ).map(result => toTestErrors(result._1))
}
