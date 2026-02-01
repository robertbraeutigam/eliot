package com.vanillasource.eliot.eliotc.resolve2

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName => ModuleName2, ValueFQN}
import com.vanillasource.eliot.eliotc.module2.processor.*
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve2.processor.ValueResolver
import com.vanillasource.eliot.eliotc.resolve2.ExpressionMatchers.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

class ValueResolverTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq(ModuleName2.systemFunctionModuleName)),
      UnifiedModuleValueProcessor(),
      ValueResolver()
    ) {
  private val testModuleName2   = ModuleName2(Seq.empty, "Test")
  private val functionModuleName = ModuleName2.systemFunctionModuleName

  "value resolver" should "resolve a literal integer expression" in {
    runEngineForValue("data T\na: T = 1").flatMap {
      case Some(IntegerLiteral(Sourced(_, _, value))) => IO.delay(value shouldBe BigInt(1))
      case x                                          => IO.delay(fail(s"was not an integer literal, instead: $x"))
    }
  }

  it should "resolve abstract values" in {
    runEngineForErrors("data T\na: T").asserting(_ shouldBe Seq())
  }

  it should "resolve a string literal expression" in {
    runEngineForValue("data T\na: T = \"hello\"").flatMap {
      case Some(StringLiteral(Sourced(_, _, value))) => IO.delay(value shouldBe "hello")
      case x                                         => IO.delay(fail(s"was not a string literal, instead: $x"))
    }
  }

  it should "resolve value references" in {
    runEngineForValue("data T\nb: T\na: T = b").flatMap {
      case Some(ValueReference(Sourced(_, _, vfqn))) =>
        IO.delay(vfqn shouldBe ValueFQN(testModuleName2, "b"))
      case x                                         => IO.delay(fail(s"was not a value reference, instead: $x"))
    }
  }

  it should "resolve lambda parameter references" in {
    runEngineForValue("data T\na: T = x: T -> x").flatMap {
      case Some(FunctionLiteral(_, _, Sourced(_, _, body))) =>
        body.signature match {
          case ParameterReference(Sourced(_, _, name)) => IO.delay(name shouldBe "x")
          case _                                       => IO.delay(fail(s"body was not a parameter reference"))
        }
      case x                                                =>
        IO.delay(fail(s"was not a function literal with parameter reference, instead: $x"))
    }
  }

  it should "resolve abstract functions' signature" in {
    runEngineForSignature("data T\na(x: T): T").flatMap {
      case Some(FunApp(FunApp(ValRef(fnVfqn), ValRef(argVfqn)), ValRef(retVfqn))) =>
        IO.delay {
          fnVfqn shouldBe ValueFQN(functionModuleName, "Function$DataType")
          argVfqn shouldBe ValueFQN(testModuleName2, "T$DataType")
          retVfqn shouldBe ValueFQN(testModuleName2, "T$DataType")
        }
      case x                                                                      =>
        IO.delay(fail(s"was not a function application, instead: $x"))
    }
  }

  it should "resolve function application" in {
    runEngineForValue("data T\nf: T\nb: T\na: T = f(b)").flatMap {
      case Some(FunctionApplication(Sourced(_, _, target), _)) =>
        target.signature match {
          case ValueReference(_) => IO.pure(succeed)
          case _                 => IO.delay(fail(s"target was not a value reference"))
        }
      case x                                                   =>
        IO.delay(fail(s"was not a function application, instead: $x"))
    }
  }

  it should "resolve qualified value reference" in {
    runEngineForValue("data T\nb: T\na: T = Test::b").flatMap {
      case Some(ValueReference(Sourced(_, _, vfqn))) =>
        IO.delay(vfqn shouldBe ValueFQN(testModuleName2, "b"))
      case x                                         => IO.delay(fail(s"was not a value reference, instead: $x"))
    }
  }

  it should "report error for undefined qualified name" in {
    runEngineForErrors("data T\na: T = NonExistent::value")
      .asserting(_ shouldBe Seq("Name not defined."))
  }

  it should "report error for undefined name" in {
    runEngineForErrors("data T\na: T = undefined").asserting(_ shouldBe Seq("Name not defined."))
  }

  it should "report error when lambda parameter shadows dictionary name" in {
    runEngineForErrors("data T\nb: T\na: T = b: T -> b")
      .asserting(_ shouldBe Seq("Parameter shadows existing name in scope."))
  }

  it should "report error when nested lambda parameter shadows outer parameter" in {
    runEngineForErrors("data T\na: T = x: T -> x: T -> x")
      .asserting(_ shouldBe Seq("Parameter shadows existing name in scope."))
  }

  it should "resolve type expressions" in {
    runEngineForTypeExpression("data SomeType(s: SomeType)\na: SomeType").flatMap {
      case ValueReference(Sourced(_, _, vfqn)) =>
        IO.delay(vfqn shouldBe ValueFQN(testModuleName2, "SomeType$DataType"))
      case x                                   => IO.delay(fail(s"type was not resolved to value reference, instead: $x"))
    }
  }

  it should "allow same parameter name in sibling lambdas" in {
    runEngineForErrors("data T(t: T)\nf: T\na: T = f(x: T -> x, x: T -> x)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not leak lambda parameter to following argument" in {
    runEngineForErrors("data T(t: T)\nf: T\nb: T\na: T = f(x: T -> x, b)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not leak lambda parameter to outer scope" in {
    runEngineForErrors("data T(t: T)\nf: T\na: T = f(x: T -> x, x)")
      .asserting(_ shouldBe Seq("Name not defined."))
  }

  private def runEngineForValue(source: String): IO[Option[Expression]] =
    runGenerator(source, ResolvedValue.Key(ValueFQN(testModuleName2, "a")), systemImports).map { case (errors, facts) =>
      facts.values
        .collectFirst { case rv: ResolvedValue if rv.vfqn.name == "a" => rv }
        .flatMap(_.runtime.map(_.value))
    }

  private def runEngineForSignature(source: String): IO[Option[Expression]] =
    runGenerator(source, ResolvedValue.Key(ValueFQN(testModuleName2, "a")), systemImports).map { case (errors, facts) =>
      facts.values
        .collectFirst { case rv: ResolvedValue if rv.vfqn.name == "a" => rv }
        .map(_.typeStack.value.signature)
    }

  private def runEngineForTypeExpression(source: String): IO[Expression] =
    runGenerator(source, ResolvedValue.Key(ValueFQN(testModuleName2, "a")), systemImports).map { case (_, facts) =>
      facts.values
        .collectFirst { case rv: ResolvedValue if rv.vfqn.name == "a" => rv }
        .map(_.typeStack.value.signature)
        .get
    }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(source, ResolvedValue.Key(ValueFQN(testModuleName2, "a")), systemImports).map(_._1.map(_.message))
}
