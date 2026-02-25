package com.vanillasource.eliot.eliotc.resolve

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName => ModuleName2, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, Fixity, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.resolve.ExpressionMatchers.*
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
  private val testModuleName2    = ModuleName2(Seq.empty, "Test")
  private val functionModuleName = ModuleName2.systemFunctionModuleName

  "value resolver" should "resolve a literal integer expression" in {
    runEngineForValue("data T\ndef a: T = 1").flatMap {
      case Some(IntegerLiteral(Sourced(_, _, value))) => IO.delay(value shouldBe BigInt(1))
      case x                                          => IO.delay(fail(s"was not an integer literal, instead: $x"))
    }
  }

  it should "resolve abstract values" in {
    runEngineForErrors("data T\ndef a: T").asserting(_ shouldBe Seq())
  }

  it should "resolve a string literal expression" in {
    runEngineForValue("data T\ndef a: T = \"hello\"").flatMap {
      case Some(StringLiteral(Sourced(_, _, value))) => IO.delay(value shouldBe "hello")
      case x                                         => IO.delay(fail(s"was not a string literal, instead: $x"))
    }
  }

  it should "resolve value references" in {
    runEngineForValue("data T\ndef b: T\ndef a: T = b").flatMap {
      case Some(ValueReference(Sourced(_, _, vfqn), _)) =>
        IO.delay(vfqn shouldBe ValueFQN(testModuleName2, QualifiedName("b", Qualifier.Default)))
      case x                                         => IO.delay(fail(s"was not a value reference, instead: $x"))
    }
  }

  it should "resolve lambda parameter references" in {
    runEngineForValue("data T\ndef a: T = x: T -> x").flatMap {
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
    runEngineForSignature("data T\ndef a(x: T): T").flatMap {
      case Some(FunApp(FunApp(ValRef(fnVfqn), ValRef(argVfqn)), ValRef(retVfqn))) =>
        IO.delay {
          fnVfqn shouldBe ValueFQN(functionModuleName, QualifiedName("Function", Qualifier.Type))
          argVfqn shouldBe ValueFQN(testModuleName2, QualifiedName("T", Qualifier.Type))
          retVfqn shouldBe ValueFQN(testModuleName2, QualifiedName("T", Qualifier.Type))
        }
      case x                                                                      =>
        IO.delay(fail(s"was not a function application, instead: $x"))
    }
  }

  it should "resolve function application" in {
    runEngineForValue("data T\ndef f: T\ndef b: T\ndef a: T = f(b)").flatMap {
      case Some(FunctionApplication(Sourced(_, _, target), _)) =>
        target.signature match {
          case ValueReference(_, _) => IO.pure(succeed)
          case _                 => IO.delay(fail(s"target was not a value reference"))
        }
      case x                                                   =>
        IO.delay(fail(s"was not a function application, instead: $x"))
    }
  }

  it should "resolve qualified value reference" in {
    runEngineForValue("data T\ndef b: T\ndef a: T = Test::b").flatMap {
      case Some(ValueReference(Sourced(_, _, vfqn), _)) =>
        IO.delay(vfqn shouldBe ValueFQN(testModuleName2, QualifiedName("b", Qualifier.Default)))
      case x                                         => IO.delay(fail(s"was not a value reference, instead: $x"))
    }
  }

  it should "report error for undefined qualified name" in {
    runEngineForErrors("data T\ndef a: T = NonExistent::value")
      .asserting(_ shouldBe Seq("Qualified named value not available."))
  }

  it should "report error for undefined name" in {
    runEngineForErrors("data T\ndef a: T = undefined").asserting(_ shouldBe Seq("Name not defined."))
  }

  it should "allow lambda parameter to shadow dictionary name" in {
    runEngineForValue("data T\ndef b: T\ndef a: T = b: T -> b").flatMap {
      case Some(FunctionLiteral(_, _, Sourced(_, _, body))) =>
        body.signature match {
          case ParameterReference(Sourced(_, _, name)) => IO.delay(name shouldBe "b")
          case _                                       => IO.delay(fail(s"body was not a parameter reference"))
        }
      case x                                                =>
        IO.delay(fail(s"was not a function literal with parameter reference, instead: $x"))
    }
  }

  it should "allow nested lambda parameter to shadow outer parameter" in {
    runEngineForValue("data T\ndef a: T = x: T -> x: T -> x").flatMap {
      case Some(FunctionLiteral(_, _, Sourced(_, _, outerBody))) =>
        outerBody.signature match {
          case FunctionLiteral(_, _, Sourced(_, _, innerBody)) =>
            innerBody.signature match {
              case ParameterReference(Sourced(_, _, name)) => IO.delay(name shouldBe "x")
              case _                                       => IO.delay(fail(s"inner body was not a parameter reference"))
            }
          case _                                               => IO.delay(fail(s"outer body was not a function literal"))
        }
      case x                                                     =>
        IO.delay(fail(s"was not a nested function literal, instead: $x"))
    }
  }

  it should "resolve type expressions" in {
    runEngineForTypeExpression("data SomeType(s: SomeType)\ndef a: SomeType").flatMap {
      case ValueReference(Sourced(_, _, vfqn), _) =>
        IO.delay(vfqn shouldBe ValueFQN(testModuleName2, QualifiedName("SomeType", Qualifier.Type)))
      case x                                   => IO.delay(fail(s"type was not resolved to value reference, instead: $x"))
    }
  }

  it should "allow same parameter name in sibling lambdas" in {
    runEngineForErrors("data T(t: T)\ndef f: T\ndef a: T = f(x: T -> x, x: T -> x)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not leak lambda parameter to following argument" in {
    runEngineForErrors("data T(t: T)\ndef f: T\ndef b: T\ndef a: T = f(x: T -> x, b)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not leak lambda parameter to outer scope" in {
    runEngineForErrors("data T(t: T)\ndef f: T\ndef a: T = f(x: T -> x, x)")
      .asserting(_ shouldBe Seq("Name not defined."))
  }

  "flat expressions" should "pass through as FlatExpression in resolved value" in {
    runEngineForValue("data T\ndef b: T\ndef c: T\ndef a: T = b c").flatMap {
      case Some(FlatExpression(parts)) => IO.pure(parts.length shouldBe 2)
      case x                          => IO.delay(fail(s"was not a FlatExpression, instead: $x"))
    }
  }

  "fixity" should "be Application by default" in {
    runEngineForFixity("data T\ndef b: T\ndef a: T = b").asserting(_ shouldBe Fixity.Application)
  }

  it should "be Prefix when declared with prefix" in {
    runEngineForFixity("data T\ndef b: T\nprefix def a: T = b").asserting(_ shouldBe Fixity.Prefix)
  }

  it should "be Infix Left when declared with infix" in {
    runEngineForFixity("data T\ndef b: T\ninfix def a: T = b")
      .asserting(_ shouldBe Fixity.Infix(Fixity.Associativity.Left))
  }

  it should "be Postfix when declared with postfix" in {
    runEngineForFixity("data T\ndef b: T\npostfix def a: T = b").asserting(_ shouldBe Fixity.Postfix)
  }

  private def runEngineForFixity(source: String): IO[Fixity] =
    runGenerator(
      source,
      ResolvedValue.Key(ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default))),
      systemImports
    ).map { case (_, facts) =>
      facts.values
        .collectFirst { case rv: ResolvedValue if rv.vfqn.name == QualifiedName("a", Qualifier.Default) => rv }
        .map(_.fixity)
        .get
    }

  private def runEngineForValue(source: String): IO[Option[Expression]] =
    runGenerator(
      source,
      ResolvedValue.Key(ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default))),
      systemImports
    ).map { case (errors, facts) =>
      facts.values
        .collectFirst { case rv: ResolvedValue if rv.vfqn.name == QualifiedName("a", Qualifier.Default) => rv }
        .flatMap(_.runtime.map(_.value))
    }

  private def runEngineForSignature(source: String): IO[Option[Expression]] =
    runGenerator(
      source,
      ResolvedValue.Key(ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default))),
      systemImports
    ).map { case (errors, facts) =>
      facts.values
        .collectFirst { case rv: ResolvedValue if rv.vfqn.name == QualifiedName("a", Qualifier.Default) => rv }
        .map(_.typeStack.value.signature)
    }

  private def runEngineForTypeExpression(source: String): IO[Expression] =
    runGenerator(
      source,
      ResolvedValue.Key(ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default))),
      systemImports
    ).map { case (_, facts) =>
      facts.values
        .collectFirst { case rv: ResolvedValue if rv.vfqn.name == QualifiedName("a", Qualifier.Default) => rv }
        .map(_.typeStack.value.signature)
        .get
    }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(
      source,
      ResolvedValue.Key(ValueFQN(testModuleName2, QualifiedName("a", Qualifier.Default))),
      systemImports
    ).map(_._1.map(_.message))
}
