package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.pos.Sourced

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

  it should "not parse import that is not top-level, even if correct" in {
    runEngineForErrors(" import a.b.C").asserting(
      _ shouldBe Seq("Expected function name or top level keyword 'data', but encountered keyword 'import'.")
    )
  }

  it should "report multiple errors, not fail on the first one" in {
    runEngineForErrors("import a.b\nimport c.d").asserting(_.size shouldBe 2)
  }

  it should "reject keyword 'import' as function name" in {
    runEngineForErrors("a: Byte = b\nimport = a").asserting(_.size should be > 0)
  }

  it should "accept a constant definition without parentheses" in {
    runEngineForErrors("a: Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "reject a definition without type" in {
    runEngineForErrors("a = b").asserting(_.size should be > 0)
  }

  it should "reject a constant definition with empty parentheses for empty args" in {
    runEngineForErrors("a(): Byte = b").asserting(_ shouldBe Seq("Expected argument name, but encountered symbol ')'."))
  }

  it should "accept a function definition with one argument" in {
    runEngineForErrors("a(b: Byte): Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function definition with two arguments" in {
    runEngineForErrors("a(b: Byte, c: Byte): Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "reject argument list without the comma separate" in {
    runEngineForErrors("a(b: Byte c: Byte): Byte = b").asserting(
      _ shouldBe Seq("Expected symbol '[', symbol ',' or symbol ')', but encountered identifier 'c'.")
    )
  }

  it should "accept a function definition with three arguments" in {
    runEngineForErrors("a(b: Byte, c: Byte, d: Byte): Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "reject even if one parameter does not have a type definition" in {
    runEngineForErrors("a(b: Byte, c, d: Byte): Byte = b").asserting(_.size should be > 0)
  }

  it should "accept a function application without parameters" in {
    runEngineForErrors("a: Byte = b").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application with 1 parameters" in {
    runEngineForErrors("a: Byte = b(c)").asserting(_ shouldBe Seq.empty)
  }

  it should "reject a function application with empty parameter list" in {
    runEngineForErrors("a: Byte = b()").asserting(
      _ shouldBe Seq("Expected function name, integer literal or string literal, but encountered symbol ')'.")
    )
  }

  it should "accept a function application with 2 parameters" in {
    runEngineForErrors("a: Byte = b(c, d)").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application with 3 parameters" in {
    runEngineForErrors("a: Byte = b(c, d, e)").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application with 2 parameters, one is an integer literal" in {
    runEngineForErrors("a: Byte = b(c, 1)").asserting(_ shouldBe Seq.empty)
  }

  it should "accept one generic type parameter" in {
    runEngineForErrors("a[A]: A").asserting(_ shouldBe Seq.empty)
  }

  it should "accept two generic type parameters, separated by comma" in {
    runEngineForErrors("a[A, B]: A").asserting(_ shouldBe Seq.empty)
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
    runEngineForErrors("data A[B, C]\nf[B, C]: A[B, C]").asserting(_ shouldBe Seq.empty)
  }

  it should "accept generic type parameters in parameter definitions" in {
    runEngineForErrors("data A[B, C]\nf[B, C](p: A[B, C]): C").asserting(_ shouldBe Seq.empty)
  }

  it should "not accept data definition with lower case" in {
    runEngineForErrors("data a").asserting(_ shouldBe Seq("Expected type name, but encountered identifier 'a'."))
  }

  it should "parse function literal with one parameter without parenthesis" in {
    runEngineForErrors("f: A = a:A -> a").asserting(_ shouldBe Seq.empty)
  }

  it should "parse function literal with one parameter with parenthesis" in {
    runEngineForErrors("f: A = (a:A) -> a").asserting(_ shouldBe Seq.empty)
  }

  it should "parse qualified function application without arguments" in {
    runEngineForErrors("f: A = eliot.lang.String::println").asserting(_ shouldBe Seq.empty)
  }

  it should "parse qualified function application with one argument" in {
    runEngineForErrors("f: A = eliot.lang.String::println(a)").asserting(_ shouldBe Seq.empty)
  }

  it should "parse qualified function application with two arguments" in {
    runEngineForErrors("f: A = eliot.lang.String::println(a, b)").asserting(_ shouldBe Seq.empty)
  }

  it should "parse qualified function application with single part module name" in {
    runEngineForErrors("f: A = HelloWorld::main").asserting(_ shouldBe Seq.empty)
  }

  it should "reject qualified function application with empty parameter list" in {
    runEngineForErrors("f: A = eliot.lang.String::println()").asserting(_.size should be > 0)
  }

  private def runEngine(source: String): IO[Map[CompilerFactKey[?], CompilerFact]] =
    runGenerator(source, SourceAST.Key(file))

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGeneratorForErrors(source, SourceAST.Key(file))

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
}
