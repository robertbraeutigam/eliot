package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.{CompilerFact, ProcessorTest}
import com.vanillasource.eliot.eliotc.main.CompilerEngine
import com.vanillasource.eliot.eliotc.source.{SourceContent, Sourced, SourcedError}
import com.vanillasource.eliot.eliotc.token.Tokenizer
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class ASTParserTest extends ProcessorTest(new Tokenizer(), new ASTParser()) {
  "ast parser" should "successfully parse empty file" in {
    runEngineForErrors("").asserting(_ shouldBe Seq.empty)
  }

  it should "parse a correct import statement" in {
    parseForImports("import a.b.C").asserting(_ shouldBe Seq("a.b.C"))
  }

  it should "parse multiple current import statements" in {
    parseForImports("import a.b.C\nimport b.D\nimport g.g.H").asserting(_ shouldBe Seq("a.b.C", "b.D", "g.g.H"))
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
      _ shouldBe Seq("Expected symbol ',' or symbol ')', but encountered identifier 'c'.")
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
      _ shouldBe Seq("Expected function name or integer literal, but encountered symbol ')'.")
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

  private def parseForImports(source: String): IO[Seq[String]] = for {
    results <- runEngine(source)
  } yield {
    results.values
      .collect { case SourceAST(_, AST(statements, _, _)) =>
        statements.map(i => (i.packageNames.map(_.value.content) :+ i.moduleName.value.content).mkString("."))
      }
      .toSeq
      .flatten
  }
}
