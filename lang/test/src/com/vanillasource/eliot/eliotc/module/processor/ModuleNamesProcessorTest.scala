package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module.fact.ModuleNames
import com.vanillasource.eliot.eliotc.token.Tokenizer

class ModuleNamesProcessorTest
    extends ProcessorTest(Tokenizer(), ASTParser(), CoreProcessor(), ModuleNamesProcessor()) {

  "module names processor" should "extract a single name" in {
    runEngineForNames("def a: A").asserting(_ shouldBe Set(QualifiedName("a", Qualifier.Default)))
  }

  it should "extract multiple names" in {
    runEngineForNames("def a: A\ndef b: B").asserting(
      _ shouldBe Set(QualifiedName("a", Qualifier.Default), QualifiedName("b", Qualifier.Default))
    )
  }

  it should "extract names from data definitions" in {
    runEngineForNames("data Person").asserting(_ shouldBe Set(QualifiedName("Person", Qualifier.Type)))
  }

  it should "extract constructor and accessor names from data with fields" in {
    runEngineForNames("data Person(name: Name)").asserting(
      _ shouldBe Set(
        QualifiedName("Person", Qualifier.Type),
        QualifiedName("Person", Qualifier.Default),
        QualifiedName("name", Qualifier.Default),
        QualifiedName("handlePersonWith", Qualifier.Default)
      )
    )
  }

  it should "extract mixed function and data names" in {
    runEngineForNames("data A\ndef f: A").asserting(
      _ shouldBe Set(QualifiedName("A", Qualifier.Type), QualifiedName("f", Qualifier.Default))
    )
  }

  it should "detect duplicate names" in {
    runEngineForErrors("def a: A\ndef a: B").asserting(_ shouldBe Seq("Name was already defined in this module." at "a"))
  }

  it should "return empty set for empty module" in {
    runEngineForNames("").asserting(_ shouldBe Set())
  }

  private def runEngineForNames(source: String): IO[Set[QualifiedName]] =
    runGenerator(source, ModuleNames.Key(file)).map { case (_, facts) =>
      facts.values.collectFirst { case ModuleNames(_, names) => names.value.keySet }.getOrElse(Set.empty)
    }

  private def runEngineForErrors(source: String): IO[Seq[TestError]] =
    runGenerator(source, ModuleNames.Key(file)).map(result => toTestErrors(result._1))
}
