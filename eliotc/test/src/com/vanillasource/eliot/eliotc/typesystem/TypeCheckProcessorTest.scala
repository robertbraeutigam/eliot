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
    runEngineForErrors("data A\na: A = b\nb: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not compile if call site has arguments, but definition doesn't" in {
    runEngineForErrors("data A\na: A = b(1)\nb: A")
      .asserting(_ shouldBe Seq("Function is called with 1 parameters, but needs 0."))
  }

  it should "issue error when referencing an undefined function" in {
    runEngineForErrors("data A\na: A = c")
      .asserting(_ shouldBe Seq("Function not defined."))
  }

  it should "not compile if call site has no arguments, but definition has one" in {
    runEngineForErrors("data A\na: A = b\nb(x: A): A")
      .asserting(_ shouldBe Seq("Function is called with 0 parameters, but needs 1."))
  }

  "processor" should "produce type checked results if arities are ok" in {
    runForTypedFunctions("data A\na: A = b\nb: A")
      .asserting(_.length shouldBe 1)
  }

  it should "not produce type checked results if arities mismatch" in {
    runForTypedFunctions("data A\na: A = b(3)\nb: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail only once when a function is used wrong" in {
    runEngineForErrors("data A\ndata B\na: A\nb: B = a")
      .asserting(_ shouldBe Seq("Expression with type Test.A can not be assigned to type Test.B."))
  }

  it should "fail if parameter is of wrong type" in {
    runEngineForErrors("data A\ndata B\na(b: B): A = b")
      .asserting(_ shouldBe Seq("Expression with type Test.B can not be assigned to type Test.A."))
  }

  it should "fail if parameter is used as a wrong parameter in another function" in {
    runEngineForErrors("data A\ndata B\na(b: B): A\nb(x: A): A = a(x)")
      .asserting(_ shouldBe Seq("Expression with type Test.A can not be assigned to type Test.B."))
  }

  "generic types" should "type check when returning itself from a parameter" in {
    runEngineForErrors("a[A](a: A): A = a")
      .asserting(_ shouldBe Seq())
  }

  it should "type check when returning different, but non-constrained generic" in {
    runEngineForErrors("a[A, B](a: A, b: B): A = b")
      .asserting(_ shouldBe Seq("No"))
  }

  it should "forward unification to concrete types" in {
    runEngineForErrors("id[A](a: A): A = a\ndata String\ndata Int\nb(i: Int, s: String): String = id(s)")
      .asserting(_ shouldBe Seq())
  }

  it should "fail if forward unification to concrete types produces conflict" in {
    runEngineForErrors("id[A](a: A): A = a\ndata String\ndata Int\nb(i: Int, s: String): String = id(i)")
      .asserting(_ shouldBe Seq("Expression with type Test.Int can not be assigned to type Test.String."))
  }

  private def runForTypedFunctions(source: String): IO[Seq[FunctionFQN]] = for {
    results <- runEngine(source)
  } yield {
    results.values.collect { case TypeCheckedFunction(ffqn, _) =>
      ffqn
    }.toSeq
  }

}
