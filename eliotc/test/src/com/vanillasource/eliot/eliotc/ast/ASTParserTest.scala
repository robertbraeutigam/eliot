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
    parseForErrors("").asserting(_ shouldBe Seq.empty)
  }

  it should "parse a correct import statement" in {
    parseForImports("import a.b.C").asserting(_ shouldBe Seq("a.b.C"))
  }

  it should "parse multiple current import statements" in {
    parseForImports("import a.b.C\nimport b.D\nimport g.g.H").asserting(_ shouldBe Seq("a.b.C", "b.D", "g.g.H"))
  }

  it should "not parse import with point at the end" in {
    parseForErrors("import a.b.c.").asserting(
      _ shouldBe Seq("Expected package name or module name, but end of input reached.")
    )
  }

  it should "not parse import with non-capitalized module name" in {
    parseForErrors("import a.b.c").asserting(
      _ shouldBe Seq("Expected symbol '.', but end of input reached.")
    )
  }

  it should "not parse import on multiple lines, even if correct" in {
    parseForErrors("import a.b\n.C").asserting(_.size should be > 0)
  }

  it should "not parse import that is not top-level, even if correct" in {
    parseForErrors(" import a.b.C").asserting(
      _ shouldBe Seq("Expected function name, but encountered keyword 'import'.")
    )
  }

  it should "report multiple errors, not fail on the first one" in {
    parseForErrors("import a.b\nimport c.d").asserting(_.size shouldBe 2)
  }

  it should "reject keyword 'import' as function name" in {
    parseForErrors("a = b\nimport = a").asserting(_.size should be > 0)
  }

  it should "accept a constant definition without parentheses" in {
    parseForErrors("a = b").asserting(_ shouldBe Seq.empty)
  }

  it should "reject a constant definition with empty parentheses for empty args" in {
    parseForErrors("a() = b").asserting(_ shouldBe Seq("Expected argument name, but encountered symbol ')'."))
  }

  it should "accept a function definition with one argument" in {
    parseForErrors("a(b) = b").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function definition with two arguments" in {
    parseForErrors("a(b, c) = b").asserting(_ shouldBe Seq.empty)
  }

  it should "reject argument list without the comma separate" in {
    parseForErrors("a(b c) = b").asserting(
      _ shouldBe Seq("Expected symbol ',' or symbol ')', but encountered identifier 'c'.")
    )
  }

  it should "accept a function definition with three arguments" in {
    parseForErrors("a(b, c, d) = b").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application without parameters" in {
    parseForErrors("a = b").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application with 1 parameters" in {
    parseForErrors("a = b(c)").asserting(_ shouldBe Seq.empty)
  }

  it should "reject a function application with empty parameter list" in {
    parseForErrors("a = b()").asserting(
      _ shouldBe Seq("Expected function name or integer literal, but encountered symbol ')'.")
    )
  }

  it should "accept a function application with 2 parameters" in {
    parseForErrors("a = b(c, d)").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application with 3 parameters" in {
    parseForErrors("a = b(c, d, e)").asserting(_ shouldBe Seq.empty)
  }

  it should "accept a function application with 2 parameters, one is an integer literal" in {
    parseForErrors("a = b(c, 1)").asserting(_ shouldBe Seq.empty)
  }

  private def parseForImports(source: String): IO[Seq[String]] = for {
    results <- runEngine(source)
  } yield {
    results.values
      .collect { case SourceAST(_, AST(statements, _)) =>
        statements.map(i => (i.packageNames.map(_.value.content) :+ i.moduleName.value.content).mkString("."))
      }
      .toSeq
      .flatten
  }
}
