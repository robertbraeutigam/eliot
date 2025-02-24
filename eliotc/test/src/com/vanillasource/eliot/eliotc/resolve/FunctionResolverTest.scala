package com.vanillasource.eliot.eliotc.resolve

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleName, ModuleProcessor}
import com.vanillasource.eliot.eliotc.resolve.Expression.{FunctionApplication, IntegerLiteral, ParameterReference}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

class FunctionResolverTest extends ProcessorTest(Tokenizer(), ASTParser(), ModuleProcessor(), FunctionResolver()) {
  "resolver" should "resolve a literal integer expression" in {
    parseForExpressions("data A\na: A = 1").flatMap {
      case Seq(IntegerLiteral(Sourced(_, _, value))) => IO.delay(value shouldBe BigInt(1))
      case x                                         => IO.delay(fail(s"was not an integer literal, instead: $x"))
    }
  }

  it should "resolve function calls to other function" in {
    parseForExpressions("data A\na: A\nb: A = a").flatMap {
      case Seq(FunctionApplication(Sourced(_, _, ffqn), Seq())) =>
        IO.delay(ffqn shouldBe FunctionFQN(ModuleName(Seq(), "Test"), "a"))
      case x                                                    => IO.delay(fail(s"was not a function application, instead: $x"))
    }
  }

  it should "resolve using parameters" in {
    parseForExpressions("data A\nb(a: A): A = a").flatMap {
      case Seq(ParameterReference(Sourced(_, _, name))) => IO.delay(name shouldBe "a")
      case x                                            => IO.delay(fail(s"was not a parameter reference, instead: $x"))
    }
  }

  it should "resolve generic types" in {
    runEngineForErrors("f[A]: A").asserting(_ shouldBe Seq())
  }

  it should "not resolve missing embedded generic type" in {
    runEngineForErrors("data Function[A, B]\nf[A]: Function[A, B]").asserting(_ shouldBe Seq("Type not defined."))
  }

  it should "resolve embedded generic type" in {
    runEngineForErrors("data Function[A, B]\nf[A, B]: Function[A, B]").asserting(_ shouldBe Seq.empty)
  }

  it should "resolve generic type with non-generic parameters" in {
    runEngineForErrors("data String\ndata Function[A, B]\nf: Function[String, String]").asserting(_ shouldBe Seq.empty)
  }

  it should "not resolve a type with generic parameters that were not given" in {
    runEngineForErrors("data Function[A, B]\nf: Function").asserting(
      _ shouldBe Seq("Incorrect number of generic parameters for type.")
    )
  }

  it should "not resolve a type with generic parameters that were not fully given" in {
    runEngineForErrors("data Function[A, B]\nf[A]: Function[A]").asserting(
      _ shouldBe Seq("Incorrect number of generic parameters for type.")
    )
  }

  it should "not resolve generic type with wrong arity as parameter" in {
    runEngineForErrors("f[A, B, C[A, B]](p: C[A]): B").asserting(
      _ shouldBe Seq("Incorrect number of generic parameters for type.")
    )
  }

  it should "not resolve generic type with wrong arity as return type" in {
    runEngineForErrors("f[A, B, C[A, B]]: C[A]").asserting(
      _ shouldBe Seq("Incorrect number of generic parameters for type.")
    )
  }

  private def parseForExpressions(source: String): IO[Seq[Expression]] = for {
    results <- runEngine(source)
  } yield {
    results.values.collect { case ResolvedFunction(_, FunctionDefinition(_, _, _, _, Some(expression))) =>
      expression
    }.toSeq
  }
}
