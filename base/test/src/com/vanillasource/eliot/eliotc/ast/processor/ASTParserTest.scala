package com.vanillasource.eliot.eliotc.ast.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.fact.{AST, FunctionDefinition, Qualifier, SourceAST}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

class ASTParserTest extends ProcessorTest(new Tokenizer(), new ASTParser()) {
  "ast parser" should "successfully parse empty file" in {
    runEngineForErrors("").asserting(_ shouldBe Seq.empty)
  }

  it should "parse a correct import statement" in {
    runEngineForImports("import a.b.C").asserting(_ shouldBe Seq("a.b.C"))
  }

  it should "parse multiple current import statements" in {
    runEngineForImports("import a.b.C\nimport b.D\nimport g.g.H").asserting(_ shouldBe Seq("a.b.C", "b.D", "g.g.H"))
  }

  it should "not parse import with point at the end" in {
    runEngineForErrors("import a.b.c.").asserting(
      _ shouldBe Seq("Expected package name or module name, but end of input reached.")
    )
  }

  it should "not parse import with non-capitalized module name" in {
    runEngineForErrors("import a.b.c").asserting(
      _ shouldBe Seq("Expected symbol '.', but end of input reached.")
    )
  }

  it should "not parse import on multiple lines, even if correct" in {
    runEngineForErrors("import a.b\n.C").asserting(_.size should be > 0)
  }

  it should "parse import that is not top-level" in {
    runEngineForErrors(" import a.b.C").asserting(
      _ shouldBe Seq.empty
    )
  }

  it should "report multiple errors, not fail on the first one" in {
    runEngineForErrors("import a.b\nimport c.d").asserting(_.size shouldBe 2)
  }

  it should "reject keyword 'import' as function name" in {
    runEngineForErrors("def a: Byte = b\nimport = a").asserting(_.size should be > 0)
  }

  it should "accept a constant definition without parentheses" in {
    runEngineForErrors("def a: Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "reject a definition without type" in {
    runEngineForErrors("def a = b").asserting(_.size should be > 0)
  }

  it should "reject a constant definition with empty parentheses for empty args" in {
    runEngineForErrors("def a(): Byte = b").asserting(
      _ shouldBe Seq("Expected argument name, but encountered symbol ')'.")
    )
  }

  it should "accept a function definition with one argument" in {
    runEngineForErrors("def a(b: Byte): Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function definition with two arguments" in {
    runEngineForErrors("def a(b: Byte, c: Byte): Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "reject argument list without the comma separate" in {
    runEngineForErrors("def a(b: Byte c: Byte): Byte = b").asserting(
      _ shouldBe Seq("Expected symbol '[', symbol ',' or symbol ')', but encountered identifier 'c'.")
    )
  }

  it should "accept a function definition with three arguments" in {
    runEngineForErrors("def a(b: Byte, c: Byte, d: Byte): Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "reject even if one parameter does not have a type definition" in {
    runEngineForErrors("def a(b: Byte, c, d: Byte): Byte = b").asserting(_.size should be > 0)
  }

  it should "accept a function application without parameters" in {
    runEngineForErrors("def a: Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application with 1 parameters" in {
    runEngineForErrors("def a: Byte = b(c)").asserting(_ shouldBe Seq.empty)
  }

  it should "reject a function application with empty parameter list" in {
    runEngineForErrors("def a: Byte = b()").asserting(
      _ shouldBe Seq("Expected function name, integer literal or string literal, but encountered symbol ')'.")
    )
  }

  it should "accept a function application with 2 parameters" in {
    runEngineForErrors("def a: Byte = b(c, d)").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application with 3 parameters" in {
    runEngineForErrors("def a: Byte = b(c, d, e)").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application with 2 parameters, one is an integer literal" in {
    runEngineForErrors("def a: Byte = b(c, 1)").asserting(_ shouldBe Seq.empty)
  }

  it should "accept one generic type parameter" in {
    runEngineForErrors("def a[A]: A").asserting(_ shouldBe Seq.empty)
  }

  it should "accept two generic type parameters, separated by comma" in {
    runEngineForErrors("def a[A, B]: A").asserting(_ shouldBe Seq.empty)
  }

  it should "accept an empty data definition" in {
    runEngineForErrors("data A").asserting(_ shouldBe Seq.empty)
  }

  it should "accept an empty data definition with empty parenthesis" in {
    runEngineForErrors("data A()").asserting(_ shouldBe Seq("Expected argument name, but encountered symbol ')'."))
  }

  it should "accept an empty data definition with generics" in {
    runEngineForErrors("data A[B, C]").asserting(_ shouldBe Seq.empty)
  }

  it should "accept return types with generic type parameters" in {
    runEngineForErrors("data A[B, C]\ndef f[B, C]: A[B, C]").asserting(_ shouldBe Seq.empty)
  }

  it should "accept generic type parameters in parameter definitions" in {
    runEngineForErrors("data A[B, C]\ndef f[B, C](p: A[B, C]): C").asserting(_ shouldBe Seq.empty)
  }

  it should "not accept data definition with lower case" in {
    runEngineForErrors("data a").asserting(_ shouldBe Seq("Expected type name, but encountered identifier 'a'."))
  }

  it should "parse function literal with one parameter without parenthesis" in {
    runEngineForErrors("def f: A = a:A -> a").asserting(_ shouldBe Seq.empty)
  }

  it should "parse function literal with one parameter with parenthesis" in {
    runEngineForErrors("def f: A = (a:A) -> a").asserting(_ shouldBe Seq.empty)
  }

  it should "parse qualified function application without arguments" in {
    runEngineForErrors("def f: A = eliot.lang.String::println").asserting(_ shouldBe Seq.empty)
  }

  it should "parse qualified function application with one argument" in {
    runEngineForErrors("def f: A = eliot.lang.String::println(a)").asserting(_ shouldBe Seq.empty)
  }

  it should "parse qualified function application with two arguments" in {
    runEngineForErrors("def f: A = eliot.lang.String::println(a, b)").asserting(_ shouldBe Seq.empty)
  }

  it should "parse qualified function application with single part module name" in {
    runEngineForErrors("def f: A = HelloWorld::main").asserting(_ shouldBe Seq.empty)
  }

  it should "reject qualified function application with empty parameter list" in {
    runEngineForErrors("def f: A = eliot.lang.String::println()").asserting(_.size should be > 0)
  }

  it should "accept an empty ability" in {
    runEngineForErrors("ability Showable {}").asserting(_ shouldBe Seq.empty)
  }

  it should "accept an ability with an abstract function" in {
    runEngineForErrors("ability Showable { def show: String }").asserting(_ shouldBe Seq.empty)
  }

  it should "accept an ability with a function with body" in {
    runEngineForErrors("ability Showable { def show: String = a }").asserting(_ shouldBe Seq.empty)
  }

  it should "accept an ability with multiple functions" in {
    runEngineForErrors("ability Showable { def show: String\ndef display: String = a }").asserting(_ shouldBe Seq.empty)
  }

  it should "accept an ability with generic parameters" in {
    runEngineForErrors("ability Functor[A] { def map: A }").asserting(_ shouldBe Seq.empty)
  }

  it should "reject an ability with lowercase name" in {
    runEngineForErrors("ability showable {}").asserting(
      _ shouldBe Seq("Expected ability name, but encountered identifier 'showable'.")
    )
  }

  it should "reject an ability without braces" in {
    runEngineForErrors("ability Showable").asserting(_.size should be > 0)
  }

  it should "qualify ability functions with the ability name" in {
    runEngineForFunctions("ability Showable { def show: String }").asserting(
      _ shouldBe Seq(("show", Qualifier.Ability("Showable")))
    )
  }

  it should "qualify multiple ability functions with the ability name" in {
    runEngineForFunctions("ability Showable { def show: String\ndef display: String }").asserting(
      _ shouldBe Seq(("show", Qualifier.Ability("Showable")), ("display", Qualifier.Ability("Showable")))
  )
  }

  it should "prepend ability generic parameters to function generic parameters" in {
    runEngineForFunctionGenericCounts("ability Functor[A] { def map[B]: A }").asserting(
      _ shouldBe Seq(("map", 2))
    )
  }

  it should "not mix ability functions with top-level functions" in {
    runEngineForFunctions("def f: A = a\nability Showable { def show: String }").asserting(
      _ shouldBe Seq(("f", Qualifier.Default), ("show", Qualifier.Ability("Showable")))
    )
  }

  it should "accept an empty implement block" in {
    runEngineForErrors("implement Showable {}").asserting(_ shouldBe Seq.empty)
  }

  it should "accept an implement block with a function" in {
    runEngineForErrors("implement Showable { def show: String = a }").asserting(_ shouldBe Seq.empty)
  }

  it should "accept an implement block with an abstract function" in {
    runEngineForErrors("implement Showable { def show: String }").asserting(_ shouldBe Seq.empty)
  }

  it should "accept an implement block with multiple functions" in {
    runEngineForErrors("implement Showable { def show: String = a\ndef display: String = b }").asserting(
      _ shouldBe Seq.empty
    )
  }

  it should "accept an implement block with generic parameters" in {
    runEngineForErrors("implement Functor[Byte] { def map: Byte = a }").asserting(_ shouldBe Seq.empty)
  }

  it should "reject an implement block with lowercase name" in {
    runEngineForErrors("implement showable {}").asserting(
      _ shouldBe Seq("Expected ability name, but encountered identifier 'showable'.")
    )
  }

  it should "reject an implement block without braces" in {
    runEngineForErrors("implement Showable").asserting(_.size should be > 0)
  }

  it should "qualify implement functions with AbilityImplementation qualifier" in {
    runEngineForFunctions("implement Showable { def show: String = a }").asserting(
      _ shouldBe Seq(("show", Qualifier.AbilityImplementation("Showable", Seq.empty)))
    )
  }

  it should "qualify multiple implement functions with the same qualifier" in {
    runEngineForFunctions("implement Showable { def show: String\ndef display: String }").asserting(
      _ shouldBe Seq(
        ("show", Qualifier.AbilityImplementation("Showable", Seq.empty)),
        ("display", Qualifier.AbilityImplementation("Showable", Seq.empty))
      )
    )
  }

  it should "not prepend implement generic parameters to function generic parameters" in {
    runEngineForFunctionGenericCounts("implement Functor[Byte] { def map[B]: Byte }").asserting(
      _ shouldBe Seq(("map", 1))
    )
  }

  it should "not mix implement functions with top-level functions" in {
    runEngineForFunctions("def f: A = a\nimplement Showable { def show: String = b }").asserting(
      _ shouldBe Seq(("f", Qualifier.Default), ("show", Qualifier.AbilityImplementation("Showable", Seq.empty)))
    )
  }

  private def runEngine(source: String): IO[Map[CompilerFactKey[?], CompilerFact]] =
    runGenerator(source, SourceAST.Key(file)).map(_._2)

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(source, SourceAST.Key(file)).map(_._1.map(_.message))

  private def runEngineForImports(source: String): IO[Seq[String]] =
    for {
      results <- runEngine(source)
    } yield {
      results.values
        .collect { case SourceAST(_, Sourced(_, _, AST(statements, _, _))) =>
          statements.map(i => (i.packageNames.map(_.value) :+ i.moduleName.value).mkString("."))
        }
        .toSeq
        .flatten
    }

  private def runEngineForFunctions(source: String): IO[Seq[(String, Qualifier)]] =
    for {
      results <- runEngine(source)
    } yield {
      results.values
        .collect { case SourceAST(_, Sourced(_, _, AST(_, functions, _))) =>
          functions.map(f => (f.name.value.name, f.name.value.qualifier))
        }
        .toSeq
        .flatten
    }

  private def runEngineForFunctionGenericCounts(source: String): IO[Seq[(String, Int)]] =
    for {
      results <- runEngine(source)
    } yield {
      results.values
        .collect { case SourceAST(_, Sourced(_, _, AST(_, functions, _))) =>
          functions.map(f => (f.name.value.name, f.genericParameters.size))
        }
        .toSeq
        .flatten
    }
}
