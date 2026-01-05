package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.systemFunctionModuleName
import com.vanillasource.eliot.eliotc.module.processor.{
  ModuleDataProcessor,
  ModuleFunctionProcessor,
  ModuleNamesProcessor,
  UnifiedModuleDataProcessor,
  UnifiedModuleFunctionProcessor,
  UnifiedModuleNamesProcessor
}
import com.vanillasource.eliot.eliotc.resolve.processor.FunctionResolver
import com.vanillasource.eliot.eliotc.sugar.DesugarProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.typesystem.fact.TypeCheckedFunction
import com.vanillasource.eliot.eliotc.typesystem.processor.TypeCheckProcessor

class TypeCheckProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      DesugarProcessor(),
      ModuleFunctionProcessor(Seq(systemFunctionModuleName)),
      ModuleDataProcessor(Seq(systemFunctionModuleName)),
      ModuleNamesProcessor(),
      UnifiedModuleFunctionProcessor(),
      UnifiedModuleDataProcessor(),
      UnifiedModuleNamesProcessor(),
      FunctionResolver(),
      TypeCheckProcessor()
    ) {
  "function call" should "compile if same number of arguments" in {
    runEngineForErrorsWithImports("data A\nf: A = b\nb: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not compile if call site has arguments, but definition doesn't" in {
    runEngineForErrorsWithImports("data A\nf: A = b(1)\nb: A")
      .asserting(
        _ shouldBe Seq("Target of function application is not a Function. Possibly too many arguments.")
      )
  }

  it should "issue error when referencing an undefined function" in {
    runEngineForErrorsWithImports("data A\nf: A = c")
      .asserting(_ shouldBe Seq("Function not defined."))
  }

  it should "not compile if call site has no arguments, but definition has one" in {
    runEngineForErrorsWithImports("data A\nf: A = b\nb(x: A): A")
      .asserting(
        _ shouldBe Seq("Type mismatch.")
      )
  }

  "processor" should "produce type checked results if arities are ok" in {
    runEngineForTypedFunctions("data A\nf: A = b\nb: A")
      .asserting(_.length shouldBe 1)
  }

  it should "not produce type checked results if arities mismatch" in {
    runEngineForTypedFunctions("data A\nf: A = b(3)\nb: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail only once when a function is used wrong" in {
    runEngineForErrorsWithImports("data A\ndata B\na: A\nf: B = a")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "fail if parameter is of wrong type" in {
    runEngineForErrorsWithImports("data A\ndata B\nf(b: B): A = b")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "fail if parameter is used as a wrong parameter in another function" in {
    runEngineForErrorsWithImports("data A\ndata B\na(b: B): A\nf(x: A): A = a(x)")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  "generic types" should "type check when returning itself from a parameter" in {
    runEngineForErrorsWithImports("f[A](a: A): A = a")
      .asserting(_ shouldBe Seq())
  }

  it should "type check when returning different, but non-constrained generic" in {
    runEngineForErrorsWithImports("f[A, B](a: A, b: B): A = b")
      .asserting(
        _ shouldBe Seq("Expression with universal generic type B can not be assigned to universal generic type A.")
      )
  }

  it should "forward unification to concrete types" in {
    runEngineForErrorsWithImports("id[A](a: A): A = a\ndata String\ndata Int\nf(i: Int, s: String): String = id(s)")
      .asserting(_ shouldBe Seq())
  }

  it should "forward unification to concrete types in recursive setup" in {
    runEngineForErrorsWithImports(
      "id[A](a: A): A = a\ndata String\ndata Int\nf(i: Int, s: String): String = id(id(id(s)))"
    )
      .asserting(_ shouldBe Seq())
  }

  it should "fail if forward unification to concrete types produces conflict" in {
    runEngineForErrorsWithImports("id[A](a: A): A = a\ndata String\ndata Int\nf(i: Int, s: String): String = id(i)")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "fail if forward unification to concrete types produces conflict in recursive setup" in {
    runEngineForErrorsWithImports(
      "id[A](a: A): A = a\ndata String\ndata Int\nf(i: Int, s: String): String = id(id(id(i)))"
    )
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "unify on multiple parameters" in {
    runEngineForErrorsWithImports(
      "g[A](a: A, b: A, c: A): A = a\nsomeA[A]: A\ndata String\ndata Int\nf(i: Int, s: String): String = g(someA, someA, s)"
    )
      .asserting(_ shouldBe Seq())
  }

  it should "fail, if unifying on multiple parameters fail at later stage" in {
    runEngineForErrorsWithImports(
      "g[A](a: A, b: A, c: A): A = a\nsomeA[A]: A\ndata String\ndata Int\nf(i: Int, s: String): String = g(someA, someA, i)"
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
      "data Foo\ng(a: Foo): Foo\nf: Function[Foo, Foo] = g"
    ).asserting(_ shouldBe Seq())
  }

  // TODO: check returns typed function
  "type resolve" should "store lambda type into AST" in {
    runEngineForErrorsWithImports(
      "data String\ndata Unit\ndata Foo(l: Function[Unit, String])\ng: String\nf: Foo = Foo((unit: Unit) -> g)"
    ).asserting(_ shouldBe Seq())
  }

  "apply" should "type check and return B" in {
    runEngineForErrorsWithImports(
      "f[A, B](g: Function[A, B], a: A): B = g(a)"
    ).asserting(_ shouldBe Seq())
  }

  private def runEngineForErrorsWithImports(source: String): IO[Seq[String]] =
    runGenerator(source, TypeCheckedFunction.Key(FunctionFQN(testModuleName, "f")), systemImports)
      .map(_._1.map(_.message))

  private def runEngineForTypedFunctions(source: String): IO[Seq[FunctionFQN]] = for {
    results <- runGenerator(source, TypeCheckedFunction.Key(FunctionFQN(testModuleName, "f")), systemImports).map(_._2)
  } yield {
    results.values.collect { case TypeCheckedFunction(ffqn, _) =>
      ffqn
    }.toSeq
  }

}
