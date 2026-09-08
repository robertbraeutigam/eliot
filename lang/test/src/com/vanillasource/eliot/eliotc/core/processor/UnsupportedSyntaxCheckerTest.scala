package com.vanillasource.eliot.eliotc.core.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.CoreAST
import com.vanillasource.eliot.eliotc.token.Tokenizer

/** The effects v6 surface is parsed but rejected at core until the flag day (`docs/effects.md` §10.1 step 4). */
class UnsupportedSyntaxCheckerTest extends ProcessorTest(Tokenizer(), ASTParser(), CoreProcessor()) {
  private val effectNotSupported = "Effect declarations are not supported yet."
  private val namedNotSupported  = "Named implementations are not supported yet."
  private val withNotSupported   = "Implementation binding with 'with' is not supported yet."

  "an effect declaration" should "be rejected at its name" in {
    errors("effect Console {\n  def printLine(s: String): Unit\n}").asserting(_ shouldBe Seq(effectNotSupported at "Console"))
  }

  "a named implementation" should "be rejected at its name" in {
    errors("implement mock: Console {\n  def readLine: X = a\n}").asserting(_ shouldBe Seq(namedNotSupported at "mock"))
  }

  "with in a body" should "be rejected at the implementation name" in {
    errors("def a: X = run with mock").asserting(_ shouldBe Seq(withNotSupported at "mock"))
  }

  it should "be reported once per binding" in {
    errors("def a: X = run with m1 with m2").asserting(_ shouldBe Seq(withNotSupported at "m2", withNotSupported at "m1"))
  }

  it should "be found inside a call argument" in {
    errors("def a: X = f(run with mock)").asserting(_ shouldBe Seq(withNotSupported at "mock"))
  }

  it should "be found inside a block line" in {
    errors("def a: X = {\n  val r = run with mock\n  r\n}").asserting(_ shouldBe Seq(withNotSupported at "mock"))
  }

  "with on a parameter type" should "be rejected at the implementation name" in {
    errors("def a(body: {Console} Unit with mock): Unit").asserting(_ shouldBe Seq(withNotSupported at "mock"))
  }

  "with on a data field type" should "be rejected at the implementation name" in {
    errors("data Suite(cases: {Console | Id} Unit with mock)").asserting(_ shouldBe Seq(withNotSupported at "mock"))
  }

  "the ordinary surface" should "be untouched" in {
    errors("ability Show[T] {\n  def show(t: T): String\n}\nimplement Show[X] {\n  def show(t: X): String = s\n}")
      .asserting(_ shouldBe empty)
  }

  private def errors(source: String): IO[Seq[TestError]] =
    runGenerator(source, CoreAST.Key(file)).map(result => toTestErrors(result._1))
}
