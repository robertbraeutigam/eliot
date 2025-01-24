package com.vanillasource.eliot.eliotc.resolve

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleName, ModuleProcessor}
import com.vanillasource.eliot.eliotc.resolve.Expression.{FunctionApplication, IntegerLiteral}
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

  private def parseForExpressions(source: String): IO[Seq[Expression]] = for {
    results <- runEngine(source)
  } yield {
    results.values.collect { case ResolvedFunction(_, FunctionDefinition(_, _, _, Some(expression))) =>
      expression
    }.toSeq
  }
}
