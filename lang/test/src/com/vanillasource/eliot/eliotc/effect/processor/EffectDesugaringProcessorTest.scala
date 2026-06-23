package com.vanillasource.eliot.eliotc.effect.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.effect.fact.EffectDesugaredValue
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolvedExpressionMatchers.*
import com.vanillasource.eliot.eliotc.plugin.LangProcessors

class EffectDesugaringProcessorTest extends ProcessorTest(LangProcessors()*) {

  private val consoleModule = ModuleName(ModuleName.defaultSystemPackage, "Console")
  private val monadModule   = ModuleName(ModuleName.defaultSystemPackage, "Monad")
  private val readLineFqn   = ValueFQN(consoleModule, QualifiedName("readLine", Qualifier.Ability("Console")))
  private val printlnFqn    = ValueFQN(consoleModule, QualifiedName("println", Qualifier.Ability("Console")))
  private val flatMapFqn    = ValueFQN(monadModule, QualifiedName("flatMap", Qualifier.Ability("Monad")))

  "effect body auto-lift" should "bind a direct-style println(readLine) into flatMap(readLine, x -> println(x))" in {
    runEffectDesugar("def echo: {Console} Unit = println(readLine)").asserting {
      case Some(FunApp(FunApp(ValRef(`flatMapFqn`), ValRef(`readLineFqn`)), FunLit(x, FunApp(ValRef(`printlnFqn`), ParamRef(arg))))) =>
        arg shouldBe x
      case other => fail(s"unexpected: $other")
    }
  }

  it should "leave already-monadic flatMap(readLine, s -> println(s)) unchanged (idempotent)" in {
    runEffectDesugar(
      "import eliot.lang.Monad\ndef echo: {Console} Unit = flatMap(readLine, s -> println(s))"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(fm), ValRef(`readLineFqn`)), FunLit(s, FunApp(ValRef(`printlnFqn`), ParamRef(arg))))) =>
        (fm.name.name, arg) shouldBe ("flatMap", s)
      case other => fail(s"unexpected: $other")
    }
  }

  it should "not transform a pure function body" in {
    runEffectDesugar("def greet(name: String): String = name").asserting {
      case Some(FunLit(p, ParamRef(arg))) => arg shouldBe p
      case other                          => fail(s"unexpected: $other")
    }
  }

  it should "reject an effectful body declared under a pure (non-carrier) return type" in {
    runEffectDesugarErrors("def helper: String = println(readLine)")
      .asserting(_.map(_.message) should contain("This value performs an effect but is declared pure; declare an effect set with { ... } or return an effect carrier."))
  }

  it should "reject an effect performed but not declared in the effect set (propagation/subset check)" in {
    runEffectDesugarErrors("def bad: {Console} Unit = log(readLine)")
      .asserting(_.map(_.message) should contain("This value performs the effect 'Log' but does not declare it; add it to its { ... } effect set."))
  }

  it should "accept a body whose performed effects are all declared" in {
    runEffectDesugarErrors("def ok: {Console, Log} Unit = log(readLine)").asserting(_ shouldBe Seq.empty)
  }

  it should "propagate the Inf effect: reject a {Console} body that calls forever (undeclared Inf)" in {
    runEffectDesugarErrors("import eliot.lang.Inf\ndef bad: {Console} Unit = forever(println(readLine))")
      .asserting(_.map(_.message) should contain("This value performs the effect 'Inf' but does not declare it; add it to its { ... } effect set."))
  }

  it should "accept a {Console, Inf} body that calls forever (Inf declared)" in {
    runEffectDesugarErrors("import eliot.lang.Inf\ndef ok: {Console, Inf} Unit = forever(println(readLine))")
      .asserting(_ shouldBe Seq.empty)
  }

  // The Monad ability stub (matching `stdlib/.../Monad.els`), so the idempotency case's hand-written `flatMap` resolves.
  private val monadStub  =
    SystemImport("Monad", "ability Monad[F[_]] {\ndef flatMap[A, B](fa: F[A], f: Function[A, F[B]]): F[B]\ndef pure[A](a: A): F[A]\n}")
  // The Inf effect ability stub (matching `stdlib/.../Inf.els`), import-required (not ambient), so the propagation
  // cases above can name `forever` and `{Inf}`.
  private val infStub    =
    SystemImport("Inf", "ability Inf[F[_]] {\ndef forever(step: F[Unit]): F[Unit]\n}")
  private val allImports = systemImports :+ monadStub :+ infStub

  private def runEffectDesugar(source: String): IO[Option[OperatorResolvedExpression]] =
    runGenerator(source, EffectDesugaredValue.Key(echoVfqn(source)), allImports).map { case (_, facts) =>
      facts.values
        .collectFirst { case edv: EffectDesugaredValue if edv.value.vfqn == echoVfqn(source) => edv }
        .flatMap(_.value.runtime.map(_.value))
    }

  private def runEffectDesugarErrors(source: String) =
    runGenerator(source, EffectDesugaredValue.Key(echoVfqn(source)), allImports).map(_._1)

  /** The single value defined by each one-liner source (the name after the first `def`, before `:`/`(`). */
  private def echoVfqn(source: String): ValueFQN = {
    val name = source.linesIterator
      .map(_.trim)
      .find(_.startsWith("def "))
      .map(_.drop(4).takeWhile(c => c != ':' && c != '(' && c != ' '))
      .getOrElse("echo")
    ValueFQN(testModuleName, QualifiedName(name, Qualifier.Default))
  }
}
