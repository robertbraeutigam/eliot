package com.vanillasource.eliot.eliotc.row

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.compiler.IncrementalFactGenerator
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName as ModuleName2, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.row.RowCheckerSpike.RowResult
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.nio.file.Path

/** The effects-as-rows R1 spike suite (docs/effects-as-rows.md §8, Appendix A): each test is one of the design's
  * worked examples, run through the real pipeline to [[OperatorResolvedValue]] and row-checked by the standalone
  * [[RowCheckerSpike]] — no checker, no carriers, no types involved.
  */
class RowCheckerSpikeTest
    extends ProcessorTest(LangProcessors(systemModules = Seq(ModuleName2.systemFunctionModuleName))*) {
  private val testModule = ModuleName2(Seq.empty, "Test")

  /** The shared effect environment: one effect ability (a Console stand-in) and a pure helper. */
  private val prelude =
    """data Str
      |ability Con[F[_]] { def readLine: F[Str]
      |def printLine(s: Str): F[Str] }
      |def items: {Con} Str = readLine
      |def use(s: Str): Str = s
      |""".stripMargin

  /** The discharge environment: an effect ability `X` with its carrier and a catch-shaped discharger. */
  private val dischargePrelude =
    prelude +
      """data XCarrier[G, A]
        |ability X[F[_]] { def boom[A]: F[A] }
        |def catchX[G[_], A](computation: {X | G} A, handler: Str => G[A]): G[A]
        |def failing: {X} Str = boom
        |def h(e: Str): Str = e
        |""".stripMargin

  // --- strict propagation: effects of an argument run at the call site and join the caller's row; the callee
  // (`use`, and by extension every data-manipulating function) needs no effect declaration at all. `items.use`
  // resolves to the same `use(items)` spine at the operator phase, so dot chains are covered by the same rule. ---

  "row derivation" should "propagate an effectful argument's row through a plain callee (foldLeft-chain shape)" in {
    rowCheck(prelude + "def go: {Con} Str = use(items)", Seq("items", "use", "go"), "go")
      .asserting(_ shouldBe RowResult(Set("Con"), Set("Con")))
  }

  it should "derive an effect method reference's own ability (nullary effectful value)" in {
    rowCheck(prelude, Seq("items", "use"), "items")
      .asserting(_ shouldBe RowResult(Set("Con"), Set("Con")))
  }

  // --- the choose/pick pair: under v2 these elaborate differently (the sibling argument decides bind vs
  // pass-through — the one genuinely instantiation-dependent decision); under v3 both are the same strict rule
  // and derive identically, from declared information only. ---

  it should "derive choose(readLine, readLine) by the same strict rule regardless of sibling arguments" in {
    val source = prelude + "def choose[A](x: A, y: A): A = x\ndef echo: {Con} Str = choose(readLine, readLine)"
    rowCheck(source, Seq("choose", "echo", "items", "use"), "echo")
      .asserting(_ shouldBe RowResult(Set("Con"), Set("Con")))
  }

  it should "derive pick(readLine, pure) identically to the all-effectful sibling case" in {
    val source = prelude + "def pick[A](x: A, y: A): A = x\ndef pureStr: Str\ndef echo: {Con} Str = pick(readLine, pureStr)"
    rowCheck(source, Seq("pick", "pureStr", "echo", "items", "use"), "echo")
      .asserting(_ shouldBe RowResult(Set("Con"), Set("Con")))
  }

  // --- the leak diagnostic: per-definition, located at the offending definition — subsumes v2's
  // DeclaredPureChecker and the cryptic AbilityResolver control-effect failures. ---

  it should "report an undeclared effect as a leak on the definition (declared pure but performs)" in {
    rowCheck(prelude + "def leaky: Str = readLine", Seq("leaky", "items", "use"), "leaky")
      .asserting(_.leak shouldBe Set("Con"))
  }

  // --- discharge: a pinned (capture) slot subtracts its declared stack's entries from the argument's row —
  // structural discharge as set subtraction, decided entirely by the callee's declared signature. ---

  it should "discharge to a pure result through a catch-shaped pinned slot (sign shape, no Id anywhere)" in {
    rowCheck(dischargePrelude + "def caught: Str = catchX(failing, h)", Seq("catchX", "failing", "h", "caught"), "caught")
      .asserting(_ shouldBe RowResult(Set.empty, Set.empty))
  }

  it should "join an effectful handler's latent row through the discharger (effectful catch handler)" in {
    val source = dischargePrelude +
      "def hEff(e: Str): {Con} Str = printLine(e)\ndef caught: {Con} Str = catchX(failing, hEff)"
    rowCheck(source, Seq("catchX", "failing", "hEff", "caught"), "caught")
      .asserting(_ shouldBe RowResult(Set("Con"), Set("Con")))
  }

  it should "let undischarged residual effects ride through a partial discharge" in {
    val source = dischargePrelude + "def failLog: {X, Con} Str\ndef caught: {Con} Str = catchX(failLog, h)"
    rowCheck(source, Seq("catchX", "failLog", "h", "caught"), "caught")
      .asserting(_ shouldBe RowResult(Set("Con"), Set("Con")))
  }

  it should "discharge a two-effect stack through a nested pinned carrier (syntax-directed nesting)" in {
    val source = dischargePrelude +
      """data YCarrier[G, A]
        |ability Y[F[_]] { def bang[A]: F[A] }
        |def catchBoth[G[_], A](computation: {X, Y | G} A, handler: Str => G[A]): G[A]
        |def failTwo: {X, Y} Str
        |def caught: Str = catchBoth(failTwo, h)
        |""".stripMargin
    rowCheck(source, Seq("catchBoth", "failTwo", "h", "caught"), "caught")
      .asserting(_ shouldBe RowResult(Set.empty, Set.empty))
  }

  // --- suspension is elaboration-only: a declared-suspended slot (open row on a by-value parameter) derives
  // exactly as a strict slot — the row says the effect may run under the caller's declaration; only *when* it
  // runs (bind now vs pass the computation) differs, which is the desugar's business. ---

  it should "derive a declared-suspended slot identically to a strict slot (suspension is row-neutral)" in {
    val source  = dischargePrelude +
      """def pureStr: Str
        |def branchStrict[A](c: Str, t: A, f: A): A = t
        |def branchSusp[A](c: Str, t: {X} A, f: {X} A): {X} A
        |def useStrict: {X} Str = branchStrict(pureStr, boom, boom)
        |def useSusp: {X} Str = branchSusp(pureStr, boom, boom)
        |""".stripMargin
    val names   = Seq("pureStr", "branchStrict", "branchSusp", "useStrict", "useSusp")
    (rowCheck(source, names, "useStrict"), rowCheck(source, names, "useSusp"))
      .mapN((strict, susp) => (strict, susp))
      .asserting { case (strict, susp) => (strict, susp) shouldBe (RowResult(Set("X"), Set("X")), strict) }
  }

  // --- Inf-alikes are ordinary entries: a divergence ability rides the same union and leaks the same way. ---

  it should "propagate a forever-style ability as an ordinary row entry and flag its omission" in {
    val source = prelude +
      """ability Nf[F[_]] { def forever(step: F[Str]): F[Str] }
        |def loop: {Nf, Con} Str = forever(printLine(items))
        |def badLoop: {Con} Str = forever(printLine(items))
        |""".stripMargin
    val names  = Seq("items", "use", "loop", "badLoop")
    (rowCheck(source, names, "loop"), rowCheck(source, names, "badLoop"))
      .mapN((loop, bad) => (loop, bad))
      .asserting { case (loop, bad) =>
        (loop.leak, loop.derived, bad.leak) shouldBe (Set.empty, Set("Nf", "Con"), Set("Nf"))
      }
  }

  // --- block/`val`: the applied-lambda desugar sequences the binder's effects into the row. ---

  it should "join a val-bound effectful binder's row through the block desugar" in {
    val source = prelude + "def logged: {Con} Str = {\nval x = readLine\nprintLine(x)\n}"
    rowCheck(source, Seq("items", "use", "logged"), "logged")
      .asserting(_ shouldBe RowResult(Set("Con"), Set("Con")))
  }

  private def vfqn(name: String): ValueFQN = ValueFQN(testModule, QualifiedName(name, Qualifier.Default))

  /** Compile `source` through the real pipeline, collect the [[OperatorResolvedValue]]s of `names`, and row-check
    * `target` against that universe with the standalone spike checker.
    */
  private def rowCheck(source: String, names: Seq[String], target: String): IO[RowResult] =
    for {
      generator <- IncrementalFactGenerator.create(SequentialCompilerProcessors(processors), None)
      _         <- generator.registerFact(SourceContent(file, Sourced(file, PositionRange.zero, source)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file), Platform.Compiler))
      _         <- systemImports.traverse { imp =>
                     val modulePath = imp.moduleName.toPath
                     val impFile    = java.net.URI.create(modulePath.toString)
                     generator.registerFact(PathScan(modulePath, Seq(impFile))) >>
                       generator.registerFact(PathScan(modulePath, Seq(impFile), Platform.Compiler)) >>
                       generator.registerFact(SourceContent(impFile, Sourced(impFile, PositionRange.zero, imp.content)))
                   }
      orvs      <- names.traverse(n => generator.getFact(OperatorResolvedValue.Key(vfqn(n))))
      errors    <- generator.currentErrors()
    } yield {
      if (errors.nonEmpty) throw new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}")
      val universe = orvs.flatten.map(orv => orv.vfqn -> orv).toMap
      RowCheckerSpike
        .checkValue(vfqn(target), universe)
        .getOrElse(throw new Exception(s"No OperatorResolvedValue for '$target'"))
    }
}
