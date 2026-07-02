package com.vanillasource.eliot.eliotc.effect.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.effect.fact.EffectDesugaredValue
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolvedExpressionMatchers.*
import com.vanillasource.eliot.eliotc.plugin.LangProcessors

class EffectDesugaringProcessorTest extends ProcessorTest(LangProcessors()*) {

  private val consoleModule = ModuleName(ModuleName.effectPackage, "Console")
  private val effectModule  = ModuleName(ModuleName.effectPackage, "Effect")
  private val readLineFqn   = ValueFQN(consoleModule, QualifiedName("readLine", Qualifier.Ability("Console")))
  private val printLineFqn    = ValueFQN(consoleModule, QualifiedName("printLine", Qualifier.Ability("Console")))
  private val flatMapFqn    = ValueFQN(effectModule, QualifiedName("flatMap", Qualifier.Ability("Effect")))
  private val chooseFqn     = ValueFQN(testModuleName, QualifiedName("choose", Qualifier.Default))

  "effect body auto-lift" should "bind a direct-style printLine(readLine) into flatMap(x -> printLine(x), readLine)" in {
    runEffectDesugar("import eliot.effect.Console\ndef echo: {Console} Unit = printLine(readLine)").asserting {
      case Some(FunApp(FunApp(ValRef(`flatMapFqn`), FunLit(x, FunApp(ValRef(`printLineFqn`), ParamRef(arg)))), ValRef(`readLineFqn`))) =>
        arg shouldBe x
      case other => fail(s"unexpected: $other")
    }
  }

  it should "leave already-monadic flatMap(s -> printLine(s), readLine) unchanged (idempotent)" in {
    runEffectDesugar(
      "import eliot.effect.Console\nimport eliot.effect.Effect\ndef echo: {Console} Unit = flatMap(s -> printLine(s), readLine)"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(fm), FunLit(s, FunApp(ValRef(`printLineFqn`), ParamRef(arg)))), ValRef(`readLineFqn`))) =>
        (fm.name.name, arg) shouldBe ("flatMap", s)
      case other => fail(s"unexpected: $other")
    }
  }

  // An eliminator *branch* — a value parameter whose type is the callee's own return type (`x`/`y` of
  // `choose[A](x: A, y: A): A`, exactly the shape of `foldOption`'s `ifNone: B` / `fold`'s `whenTrue: A`) — produces
  // the result rather than consuming a value, so an effectful argument there is a *branch value*, not an action to
  // sequence. It must pass through unbound (the call becomes effectful instead), which is what lets a guard combinator
  // `orError = foldOption(o, error(msg), pure)` stay a branching effect. Sequencing it would collapse the guard to an
  // unconditional short-circuit — see `CalleeInfo.isBranchPosition` and `GuardSignatureIntegrationTest`.
  it should "not sequence an effectful eliminator branch (param type == return type)" in {
    runEffectDesugar(
      "import eliot.effect.Console\ndef viaBranch: {Console} String = choose(readLine, readLine)\ndef choose[A](x: A, y: A): A = x"
    ).asserting {
      case Some(FunApp(FunApp(ValRef(`chooseFqn`), ValRef(`readLineFqn`)), ValRef(`readLineFqn`))) => succeed
      case other => fail(s"expected choose(readLine, readLine) left unsequenced, got: $other")
    }
  }

  it should "not transform a pure function body" in {
    runEffectDesugar("def greet(name: String): String = name").asserting {
      case Some(FunLit(p, ParamRef(arg))) => arg shouldBe p
      case other                          => fail(s"unexpected: $other")
    }
  }

  it should "reject an effectful body declared under a pure (non-carrier) return type" in {
    runEffectDesugarErrors("import eliot.effect.Console\ndef helper: String = printLine(readLine)")
      .asserting(_.map(_.message) should contain("This value performs an effect but is declared pure; declare an effect set with { ... } or return an effect carrier."))
  }

  it should "reject an effect performed but not declared in the effect set (propagation/subset check)" in {
    runEffectDesugarErrors("import eliot.effect.Console\nimport eliot.effect.Log\ndef bad: {Console} Unit = log(readLine)")
      .asserting(_.map(_.message) should contain("This value performs the effect 'Log' but does not declare it; add it to its { ... } effect set."))
  }

  it should "accept a body whose performed effects are all declared" in {
    runEffectDesugarErrors("import eliot.effect.Console\nimport eliot.effect.Log\ndef ok: {Console, Log} Unit = log(readLine)").asserting(_ shouldBe Seq.empty)
  }

  it should "propagate the Inf effect: reject a {Console} body that calls forever (undeclared Inf)" in {
    runEffectDesugarErrors("import eliot.effect.Console\nimport eliot.effect.Inf\ndef bad: {Console} Unit = forever(printLine(readLine))")
      .asserting(_.map(_.message) should contain("This value performs the effect 'Inf' but does not declare it; add it to its { ... } effect set."))
  }

  it should "accept a {Console, Inf} body that calls forever (Inf declared)" in {
    runEffectDesugarErrors("import eliot.effect.Console\nimport eliot.effect.Inf\ndef ok: {Console, Inf} Unit = forever(printLine(readLine))")
      .asserting(_ shouldBe Seq.empty)
  }

  // The Effect ability stub (matching `stdlib/.../Effect.els`), so the idempotency case's hand-written `flatMap` resolves.
  private val effectStub =
    SystemImport(
      "Effect",
      "ability Effect[F[_]] {\ndef flatMap[A, B](f: Function[A, F[B]], fa: F[A]): F[B]\ndef pure[A](a: A): F[A]\ndef map[A, B](f: Function[A, B], fa: F[A]): F[B]\n}",
      ModuleName.effectPackage
    )
  // The Inf effect ability stub (matching `stdlib/.../Inf.els`), import-required (not ambient), so the propagation
  // cases above can name `forever` and `{Inf}`.
  private val infStub    =
    SystemImport("Inf", "ability Inf[F[_]] {\ndef forever(step: F[Unit]): F[Unit]\n}", ModuleName.effectPackage)
  private val allImports = systemImports :+ effectStub :+ infStub

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
