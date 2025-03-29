package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.module.processor.ModuleProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.FunctionResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer

class TypeCheckProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      ModuleProcessor(),
      FunctionResolver(),
      TypeCheckProcessor()
    ) {
  "function call" should "compile if same number of arguments" in {
    runEngineForErrorsWithImports("data A\na: A = b\nb: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not compile if call site has arguments, but definition doesn't" in {
    runEngineForErrorsWithImports("data A\na: A = b(1)\nb: A")
      .asserting(
        _ shouldBe Seq("Target of function application is not a Function. Possibly too many arguments.")
      )
  }

  it should "issue error when referencing an undefined function" in {
    runEngineForErrorsWithImports("data A\na: A = c")
      .asserting(_ shouldBe Seq("Function not defined."))
  }

  it should "not compile if call site has no arguments, but definition has one" in {
    runEngineForErrorsWithImports("data A\na: A = b\nb(x: A): A")
      .asserting(
        _ shouldBe Seq("Type mismatch.")
      )
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
    runEngineForErrorsWithImports("data A\ndata B\na: A\nb: B = a")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "fail if parameter is of wrong type" in {
    runEngineForErrorsWithImports("data A\ndata B\na(b: B): A = b")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "fail if parameter is used as a wrong parameter in another function" in {
    runEngineForErrorsWithImports("data A\ndata B\na(b: B): A\nb(x: A): A = a(x)")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  "generic types" should "type check when returning itself from a parameter" in {
    runEngineForErrorsWithImports("a[A](a: A): A = a")
      .asserting(_ shouldBe Seq())
  }

  it should "type check when returning different, but non-constrained generic" in {
    runEngineForErrorsWithImports("a[A, B](a: A, b: B): A = b")
      .asserting(
        _ shouldBe Seq("Expression with universal generic type B can not be assigned to universal generic type A.")
      )
  }

  it should "forward unification to concrete types" in {
    runEngineForErrorsWithImports("id[A](a: A): A = a\ndata String\ndata Int\nb(i: Int, s: String): String = id(s)")
      .asserting(_ shouldBe Seq())
  }

  it should "forward unification to concrete types in recursive setup" in {
    runEngineForErrorsWithImports(
      "id[A](a: A): A = a\ndata String\ndata Int\nb(i: Int, s: String): String = id(id(id(s)))"
    )
      .asserting(_ shouldBe Seq())
  }

  it should "fail if forward unification to concrete types produces conflict" in {
    runEngineForErrorsWithImports("id[A](a: A): A = a\ndata String\ndata Int\nb(i: Int, s: String): String = id(i)")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "fail if forward unification to concrete types produces conflict in recursive setup" in {
    runEngineForErrorsWithImports(
      "id[A](a: A): A = a\ndata String\ndata Int\nb(i: Int, s: String): String = id(id(id(i)))"
    )
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "unify on multiple parameters" in {
    runEngineForErrorsWithImports(
      "f[A](a: A, b: A, c: A): A = a\nsomeA[A]: A\ndata String\ndata Int\nb(i: Int, s: String): String = f(someA, someA, s)"
    )
      .asserting(_ shouldBe Seq())
  }

  it should "fail, if unifying on multiple parameters fail at later stage" in {
    runEngineForErrorsWithImports(
      "f[A](a: A, b: A, c: A): A = a\nsomeA[A]: A\ndata String\ndata Int\nb(i: Int, s: String): String = f(someA, someA, i)"
    )
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  "higher kind generic types" should "type check through single generic placeholder" in {
    runEngineForErrorsWithImports("id[A](a: A): A\nf[A, B, C[A, B]](c: C[A, B]): C[A, B] = id(c)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "reject different arities of generic parameters" in {
    runEngineForErrorsWithImports("id[B, A[B]](a: A[B]): A[B]\nf[A, B, C[A, B]](c: C[A, B]): C[A, B] = id(c)")
      .asserting(
        _ shouldBe Seq(
          "Type mismatch."
        )
      )
  }

  it should "unify generic parameters of generics via a non-parameterized generic" in {
    runEngineForErrorsWithImports(
      "data Foo\ndata Bar\nid[A](a: A): A\nf(p: Function[Bar, Foo]): Function[Foo, Bar] = id(p)"
    )
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  "top level functions" should "be assignable to function types" in {
    runEngineForErrorsWithImports(
      "data Foo\nf(a: Foo): Foo\nv: Function[Foo, Foo] = f"
    ).asserting(_ shouldBe Seq())
  }

  private def runForTypedFunctions(source: String): IO[Seq[FunctionFQN]] = for {
    results <- runEngine(source)
  } yield {
    results.values.collect { case TypeCheckedFunction(ffqn, _) =>
      ffqn
    }.toSeq
  }

}
