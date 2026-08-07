package com.vanillasource.eliot.eliotc.core.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.CoreAST
import com.vanillasource.eliot.eliotc.token.Tokenizer

/** The visibility-order rule: a file's public API must be a prefix of its declarations, so no public declaration may
  * follow a private one. Driven through the real pipeline (tokenizer → AST → core) rather than against hand-built
  * named values, because what the rule is *about* is the desugaring — that `data`, `ability` and `implement` all reduce
  * to named values carrying a visibility, and so need no per-construct arms.
  */
class VisibilityOrderCheckerTest extends ProcessorTest(Tokenizer(), ASTParser(), CoreProcessor()) {
  private val violation = "Public declaration after the private declarations starting on line 2."

  "a file with no private declarations" should "be accepted" in {
    errors("def a: X\ndef b: X").asserting(_ shouldBe empty)
  }

  "a file with only private declarations" should "be accepted" in {
    errors("private def a: X\nprivate def b: X").asserting(_ shouldBe empty)
  }

  "public declarations before private ones" should "be accepted" in {
    errors("def a: X\nprivate def b: X\nprivate def c: X").asserting(_ shouldBe empty)
  }

  "a public def after a private one" should "be rejected at the public def" in {
    errors("def a: X\nprivate def b: X\ndef c: X").asserting(_ shouldBe Seq(violation at "c"))
  }

  "every public def after a private one" should "be reported" in {
    errors("def a: X\nprivate def b: X\ndef c: X\ndef d: X")
      .asserting(_ shouldBe Seq(violation at "c", violation at "d"))
  }

  "a public data after a private def" should "be rejected once, not once per name it mints" in {
    errors("def a: X\nprivate def b: X\ndata D(field: X)").asserting(_ shouldBe Seq(violation at "D"))
  }

  "a public type alias after a private def" should "be rejected" in {
    errors("def a: X\nprivate def b: X\ntype T = X").asserting(_ shouldBe Seq(violation at "T"))
  }

  "an ability after a private def" should "be rejected — abilities are public, with no exception" in {
    errors("def a: X\nprivate def b: X\nability C[A] { def m(a: A): A }")
      .asserting(_ shouldBe Seq(violation at "C"))
  }

  "an implement block after a private def" should "be rejected — implementations are public too" in {
    errors("ability C[A] { def m(a: A): A }\nprivate def b: X\nimplement C[X] { def m(a: X): X = a }")
      .asserting(_ shouldBe Seq(violation at "C"))
  }

  "a private data" should "not report itself, since every name it mints inherits its visibility" in {
    errors("def a: X\nprivate data D(field: X)").asserting(_ shouldBe empty)
  }

  "a public def after a private data" should "be rejected" in {
    errors("def a: X\nprivate data D(field: X)\ndef c: X").asserting(_ shouldBe Seq(violation at "c"))
  }

  private def errors(source: String): IO[Seq[TestError]] =
    runGenerator(source, CoreAST.Key(file)).map(result => toTestErrors(result._1))
}
