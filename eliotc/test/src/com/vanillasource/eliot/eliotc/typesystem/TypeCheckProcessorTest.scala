package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleProcessor}
import com.vanillasource.eliot.eliotc.resolve.FunctionResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer

class TypeCheckProcessorTest
    extends ProcessorTest(Tokenizer(), ASTParser(), ModuleProcessor(), FunctionResolver(), TypeCheckProcessor()) {
  "function call" should "compile if same number of arguments" in {
    runEngineForErrors("a: Byte = b\nb: Byte = 1")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not compile if call site has arguments, but definition doesn't" in {
    runEngineForErrors("a: Byte = b(1)\nb: Byte = 1")
      .asserting(_ shouldBe Seq("Function is called with 1 parameters, but needs 0."))
  }

  it should "issue error when referencing an undefined function" in {
    runEngineForErrors("a: Byte = c")
      .asserting(_ shouldBe Seq("Function not defined."))
  }

  it should "not compile if call site has no arguments, but definition has one" in {
    runEngineForErrors("a: Byte = b\nb(x: Byte): Byte = 1")
      .asserting(_ shouldBe Seq("Function is called with 0 parameters, but needs 1."))
  }

  "processor" should "produce type checked results if arities are ok" in {
    runForTypedFunctions("a: Byte = b\nb: Byte = 1")
      .asserting(_.length shouldBe 2)
  }

  it should "not produce type checked results if arities mismatch" in {
    runForTypedFunctions("a: Byte = b(3)\nb: Byte = 1")
      .asserting(_.length shouldBe 1)
  }

  it should "display type mismatch error" in {
    runEngineForErrors("a: Word = 3")
      .asserting(_ shouldBe Seq("Function body type is Byte, but function declared to return Word."))
  }

  it should "fail only once when a function is used wrong" in {
    runEngineForErrors("a: Byte = 3\nb: Word = a")
      .asserting(_ shouldBe Seq("Function body type is Byte, but function declared to return Word."))
  }

  private def runForTypedFunctions(source: String): IO[Seq[FunctionFQN]] = for {
    results <- runEngine(source)
  } yield {
    results.values.collect { case TypeCheckedFunction(ffqn, _) =>
      ffqn
    }.toSeq
  }

}
