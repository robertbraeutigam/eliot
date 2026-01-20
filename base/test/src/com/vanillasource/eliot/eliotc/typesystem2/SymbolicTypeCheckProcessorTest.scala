package com.vanillasource.eliot.eliotc.typesystem2

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName => ModuleName2, ValueFQN}
import com.vanillasource.eliot.eliotc.module2.processor.*
import com.vanillasource.eliot.eliotc.resolve2.processor.ValueResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.typesystem2.fact.TypeCheckedValue
import com.vanillasource.eliot.eliotc.typesystem2.processor.SymbolicTypeCheckProcessor

class SymbolicTypeCheckProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq(ModuleName2.systemFunctionModuleName)),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      SymbolicTypeCheckProcessor()
    ) {
  "function call" should "compile if same number of arguments" in {
    runEngineForErrors("data A\nf: A = b\nb: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not compile if call site has arguments, but definition doesn't" in {
    runEngineForErrors("data A\nf: A = b(1)\nb: A")
      .asserting(
        _ shouldBe Seq("Target of function application is not a Function. Possibly too many arguments.")
      )
  }

  it should "issue error when referencing an undefined function" in {
    runEngineForErrors("data A\nf: A = c")
      .asserting(_ shouldBe Seq("Name not defined."))
  }

  it should "not compile if call site has no arguments, but definition has one" in {
    runEngineForErrors("data A\nf: A = b\nb(x: A): A")
      .asserting(
        _ shouldBe Seq("Type mismatch.")
      )
  }

  "processor" should "produce type checked results if arities are ok" in {
    runEngineForTypedValues("data A\nf: A = b\nb: A")
      .asserting(_.length shouldBe 1)
  }

  it should "not produce type checked results if arities mismatch" in {
    runEngineForTypedValues("data A\nf: A = b(3)\nb: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail only once when a function is used wrong" in {
    runEngineForErrors("data A\ndata B\na: A\nf: B = a")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "fail if parameter is of wrong type" in {
    runEngineForErrors("data A\ndata B\nf(b: B): A = b")
      .asserting(_ shouldBe Seq("Lambda body type mismatch."))
  }

  it should "fail if parameter is used as a wrong parameter in another function" in {
    runEngineForErrors("data A\ndata B\na(b: B): A\nf(x: A): A = a(x)")
      .asserting(_ shouldBe Seq("Argument type mismatch."))
  }

  "generic types" should "type check when returning itself from a parameter" in {
    runEngineForErrors("f[A](a: A): A = a")
      .asserting(_ shouldBe Seq())
  }

  it should "type check when returning different, but non-constrained generic" in {
    runEngineForErrors("f[A, B](a: A, b: B): A = b")
      .asserting(_ shouldBe Seq("Lambda body type mismatch."))
  }

  it should "forward unification to concrete types" in {
    runEngineForErrors("id[A](a: A): A = a\ndata String\ndata Int\nf(i: Int, s: String): String = id(s)")
      .asserting(_ shouldBe Seq("Argument type mismatch.", "Lambda body type mismatch."))
  }

  it should "forward unification to concrete types in recursive setup" in {
    runEngineForErrors(
      "id[A](a: A): A = a\ndata String\ndata Int\nf(i: Int, s: String): String = id(id(id(s)))"
    )
      .asserting(_ shouldBe Seq("Argument type mismatch.", "Lambda body type mismatch."))
  }

  it should "fail if forward unification to concrete types produces conflict" in {
    runEngineForErrors("id[A](a: A): A = a\ndata String\ndata Int\nf(i: Int, s: String): String = id(i)")
      .asserting(_ shouldBe Seq("Argument type mismatch.", "Lambda body type mismatch."))
  }

  it should "fail if forward unification to concrete types produces conflict in recursive setup" in {
    runEngineForErrors(
      "id[A](a: A): A = a\ndata String\ndata Int\nf(i: Int, s: String): String = id(id(id(i)))"
    )
      .asserting(_ shouldBe Seq("Argument type mismatch.", "Lambda body type mismatch."))
  }

  it should "unify on multiple parameters" in {
    runEngineForErrors(
      "g[A](a: A, b: A, c: A): A = a\nsomeA[A]: A\ndata String\ndata Int\nf(i: Int, s: String): String = g(someA, someA, s)"
    )
      .asserting(_ shouldBe Seq("Argument type mismatch.", "Lambda body type mismatch."))
  }

  it should "fail, if unifying on multiple parameters fail at later stage" in {
    runEngineForErrors(
      "g[A](a: A, b: A, c: A): A = a\nsomeA[A]: A\ndata String\ndata Int\nf(i: Int, s: String): String = g(someA, someA, i)"
    )
      .asserting(_ shouldBe Seq("Argument type mismatch.", "Lambda body type mismatch."))
  }

  "higher kind generic types" should "type check through single generic placeholder" in {
    runEngineForErrors("id[A](a: A): A\nf[A, B, C[A, B]](c: C[A, B]): C[A, B] = id(c)")
      .asserting(_ shouldBe Seq("Argument type mismatch.", "Lambda body type mismatch."))
  }

  it should "reject different arities of generic parameters" in {
    runEngineForErrors("id[B, A[B]](a: A[B]): A[B]\nf[A, B, C[A, B]](c: C[A, B]): C[A, B] = id(c)")
      .asserting(
        _ shouldBe Seq(
          "Symbolic application target mismatch.",
          "Symbolic application argument mismatch.",
          "Symbolic application target mismatch.",
          "Symbolic application argument mismatch."
        )
      )
  }

  it should "unify generic parameters of generics via a non-parameterized generic" in {
    runEngineForErrors(
      "data Foo\ndata Bar\nid[A](a: A): A\nf(p: Function[Bar, Foo]): Function[Foo, Bar] = id(p)"
    )
      .asserting(_ shouldBe Seq("Argument type mismatch.", "Lambda body type mismatch."))
  }

  "top level functions" should "be assignable to function types" in {
    runEngineForErrors(
      "data Foo\ng(a: Foo): Foo\nf: Function[Foo, Foo] = g"
    ).asserting(_ shouldBe Seq.empty)
  }

  // TODO: check returns typed function
  "type resolve" should "store lambda type into AST" in {
    runEngineForErrors(
      "data String\ndata Unit\ndata Foo(l: Function[Unit, String])\ng: String\nf: Foo = Foo((unit: Unit) -> g)"
    ).asserting(_ shouldBe Seq("Name not defined.", "Target of function application is not a Function. Possibly too many arguments.", "Argument type mismatch."))
  }

  "apply" should "type check and return B" in {
    runEngineForErrors(
      "f[A, B](g: Function[A, B], a: A): B = g(a)"
    ).asserting(_ shouldBe Seq.empty)
  }

  "functions without body" should "be type checked successfully with simple return type" in {
    runEngineForErrors("data A\nf: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be type checked successfully with one parameter" in {
    runEngineForErrors("data A\ndata B\nf(a: A): B")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be type checked successfully with multiple parameters" in {
    runEngineForErrors("data A\ndata B\ndata C\nf(a: A, b: B): C")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be type checked successfully with generic parameters" in {
    runEngineForErrors("f[A](a: A): A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be type checked successfully with generic parameters and multiple arguments" in {
    runEngineForErrors("f[A, B](a: A, b: B): A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "produce type checked function fact for function without body" in {
    runEngineForTypedValue("data A\nf: A")
      .asserting { func =>
        func.vfqn.name shouldBe "f"
        func.definition.body shouldBe None
      }
  }

  "parameter usage" should "type check when parameter type matches return type" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\nf(x: TypeA): TypeA = x")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail when parameter type does not match return type" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\ndata TypeB(fieldB: TypeB)\nf(x: TypeA): TypeB = x")
      .asserting(_ shouldBe Seq("Lambda body type mismatch."))
  }

  // TODO: remove this after module1 is removed
  private val testModuleName2 = ModuleName2(Seq.empty, "Test")

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(source, TypeCheckedValue.Key(ValueFQN(testModuleName2, "f")), systemImports)
      .map(_._1.map(_.message))

  private def runEngineForTypedValues(source: String): IO[Seq[ValueFQN]] =
    runGenerator(source, TypeCheckedValue.Key(ValueFQN(testModuleName2, "f")), systemImports).map { case (_, facts) =>
      facts.values.collect { case TypeCheckedValue(vfqn, _) =>
        vfqn
      }.toSeq
    }

  private def runEngineForTypedValue(source: String): IO[TypeCheckedValue] =
    runGenerator(source, TypeCheckedValue.Key(ValueFQN(testModuleName2, "f")), systemImports)
      .flatMap { case (errors, facts) =>
        if (errors.nonEmpty) {
          IO.raiseError(new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}"))
        } else {
          facts.values.collectFirst { case v: TypeCheckedValue if v.vfqn.name == "f" => v } match {
            case Some(value) => IO.pure(value)
            case None        => IO.raiseError(new Exception("No type checked value 'f' found in results"))
          }
        }
      }
}
