package com.vanillasource.eliot.eliotc.resolve

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.module.processor.{
  ModuleDataProcessor,
  ModuleFunctionProcessor,
  UnifiedModuleDataProcessor,
  UnifiedModuleFunctionProcessor
}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.{
  FunctionLiteral,
  IntegerLiteral,
  ParameterReference,
  ValueReference
}
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedFunction}
import com.vanillasource.eliot.eliotc.resolve.processor.FunctionResolver
import com.vanillasource.eliot.eliotc.sugar.DesugarProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer

class FunctionResolverTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      DesugarProcessor(),
      ModuleFunctionProcessor(Seq.empty),
      ModuleDataProcessor(Seq.empty),
      UnifiedModuleFunctionProcessor(),
      UnifiedModuleDataProcessor(),
      FunctionResolver()
    ) {

  "resolver" should "resolve a literal integer expression" in {
    runEngineForExpressions("data A\nf: A = 1").flatMap {
      case Some(IntegerLiteral(Sourced(_, _, value))) => IO.delay(value shouldBe BigInt(1))
      case x                                          => IO.delay(fail(s"was not an integer literal, instead: $x"))
    }
  }

  it should "resolve value references" in {
    runEngineForExpressions("data A\na: A\nf: A = a").flatMap {
      case Some(ValueReference(Sourced(_, _, ffqn))) =>
        IO.delay(ffqn shouldBe FunctionFQN(ModuleName(Seq(), "Test"), "a"))
      case x                                         => IO.delay(fail(s"was not a value reference, instead: $x"))
    }
  }

  it should "resolve function to function literal" in {
    runEngineForExpressions("data A\nf(a: A): A = a").flatMap {
      case Some(
            FunctionLiteral(_, Sourced(_, _, ParameterReference(Sourced(_, _, name))))
          ) =>
        IO.delay(name shouldBe "a")
      case x => IO.delay(fail(s"was not a function literal with a parameter reference as body, instead: $x"))
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

  it should "not resolve lambda with unknown parameter type" in {
    runEngineForErrors("data String\nf: String = a:A -> a").asserting(
      _ shouldBe Seq("Type not defined.")
    )
  }

  it should "not resolve lambda unknown variable/function as body" in {
    runEngineForErrors("data String\nf: String = a:String -> b").asserting(
      _ shouldBe Seq("Function not defined.")
    )
  }

  it should "not allow function literal names that are already in scope" in {
    runEngineForErrors("data String\nf(a: String): String = a:String -> a").asserting(
      _ shouldBe Seq("Name already exists in scope.")
    )
  }

  it should "resolve lambda with proper type and returning the input parameter" in {
    runEngineForErrors("data String\nf: String = a:String -> a").asserting(
      _ shouldBe Seq()
    )
  }

  it should "resolve qualified function application without arguments" in {
    runEngineForExpressions("data A\nf: A = eliot.lang.String::println").flatMap {
      case Some(ValueReference(Sourced(_, _, ffqn))) =>
        IO.delay(ffqn shouldBe FunctionFQN(ModuleName(Seq("eliot", "lang"), "String"), "println"))
      case x                                         => IO.delay(fail(s"was not a value reference, instead: $x"))
    }
  }

  it should "resolve qualified function application with one argument" in {
    runEngineForExpressions("data A\na: A\nf: A = eliot.lang.String::println(a)").flatMap {
      case Some(
            Expression.FunctionApplication(
              Sourced(_, _, ValueReference(Sourced(_, _, ffqn))),
              _
            )
          ) =>
        IO.delay(ffqn shouldBe FunctionFQN(ModuleName(Seq("eliot", "lang"), "String"), "println"))
      case x => IO.delay(fail(s"was not a function application with qualified value reference, instead: $x"))
    }
  }

  it should "resolve qualified function application with two arguments" in {
    runEngineForExpressions("data A\na: A\nb: A\nf: A = eliot.lang.String::concat(a, b)").flatMap {
      case Some(
            Expression.FunctionApplication(
              Sourced(
                _,
                _,
                Expression.FunctionApplication(
                  Sourced(_, _, ValueReference(Sourced(_, _, ffqn))),
                  _
                )
              ),
              _
            )
          ) =>
        IO.delay(ffqn shouldBe FunctionFQN(ModuleName(Seq("eliot", "lang"), "String"), "concat"))
      case x => IO.delay(fail(s"was not a nested function application with qualified value reference, instead: $x"))
    }
  }

  it should "resolve qualified function application with simple module name" in {
    runEngineForExpressions("data A\nf: A = HelloWorld::main").flatMap {
      case Some(ValueReference(Sourced(_, _, ffqn))) =>
        IO.delay(ffqn shouldBe FunctionFQN(ModuleName(Seq(), "HelloWorld"), "main"))
      case x                                         => IO.delay(fail(s"was not a value reference, instead: $x"))
    }
  }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(source, ResolvedFunction.Key(FunctionFQN(testModuleName, "f"))).map(_._1.map(_.message))

  private def runEngineForExpressions(source: String): IO[Option[Expression]] =
    for {
      facts <- runGenerator(source, ResolvedFunction.Key(FunctionFQN(testModuleName, "f"))).map(_._2)
    } yield facts
      .get(ResolvedFunction.Key(FunctionFQN(testModuleName, "f")))
      .flatMap(_.asInstanceOf[ResolvedFunction].definition.body.map(_.value))
}
