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
import com.vanillasource.eliot.eliotc.row.RowChecker.{RowResult, Universe}
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.nio.file.Path

/** Unit suite of the [[RowChecker]] (effects-as-rows R3): each test is one of the design's worked examples
  * (docs/effects-as-rows.md Appendix A), run through the real pipeline to [[OperatorResolvedValue]] and row-checked
  * standalone — no NbE checker, no carriers, no types involved. Assertions compare ability *names* (the row entries'
  * FQN tails) for readability.
  */
class RowCheckerTest
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
      .asserting(namesOf(_) shouldBe (Set("Con"), Set("Con")))
  }

  it should "derive an effect method reference's own ability (nullary effectful value)" in {
    rowCheck(prelude, Seq("items", "use"), "items")
      .asserting(namesOf(_) shouldBe (Set("Con"), Set("Con")))
  }

  // --- the choose/pick pair: under v2 these elaborate differently (the sibling argument decides bind vs
  // pass-through — the one genuinely instantiation-dependent decision); under v3 both are the same strict rule
  // and derive identically, from declared information only. ---

  it should "derive choose(readLine, readLine) by the same strict rule regardless of sibling arguments" in {
    val source = prelude + "def choose[A](x: A, y: A): A = x\ndef echo: {Con} Str = choose(readLine, readLine)"
    rowCheck(source, Seq("choose", "echo", "items", "use"), "echo")
      .asserting(namesOf(_) shouldBe (Set("Con"), Set("Con")))
  }

  it should "derive pick(readLine, pure) identically to the all-effectful sibling case" in {
    val source =
      prelude + "def pick[A](x: A, y: A): A = x\ndef pureStr: Str\ndef echo: {Con} Str = pick(readLine, pureStr)"
    rowCheck(source, Seq("pick", "pureStr", "echo", "items", "use"), "echo")
      .asserting(namesOf(_) shouldBe (Set("Con"), Set("Con")))
  }

  // --- the leak diagnostic: per-definition, located at the offending definition — subsumes v2's
  // DeclaredPureChecker and the cryptic AbilityResolver control-effect failures. ---

  it should "report an undeclared effect as a leak on the definition (declared pure but performs)" in {
    rowCheck(prelude + "def leaky: Str = readLine", Seq("leaky", "items", "use"), "leaky")
      .asserting(_.leak.map(_.abilityName) shouldBe Set("Con"))
  }

  // --- discharge: a pinned (capture) slot subtracts its declared stack's entries from the argument's row —
  // structural discharge as set subtraction, decided entirely by the callee's declared signature. ---

  it should "discharge to a pure result through a catch-shaped pinned slot (sign shape, no Id anywhere)" in {
    rowCheck(
      dischargePrelude + "def caught: Str = catchX(failing, h)",
      Seq("catchX", "failing", "h", "caught"),
      "caught"
    )
      .asserting(namesOf(_) shouldBe (Set.empty, Set.empty))
  }

  it should "join an effectful handler's latent row through the discharger (effectful catch handler)" in {
    val source = dischargePrelude +
      "def hEff(e: Str): {Con} Str = printLine(e)\ndef caught: {Con} Str = catchX(failing, hEff)"
    rowCheck(source, Seq("catchX", "failing", "hEff", "caught"), "caught")
      .asserting(namesOf(_) shouldBe (Set("Con"), Set("Con")))
  }

  it should "let undischarged residual effects ride through a partial discharge" in {
    val source = dischargePrelude + "def failLog: {X, Con} Str\ndef caught: {Con} Str = catchX(failLog, h)"
    rowCheck(source, Seq("catchX", "failLog", "h", "caught"), "caught")
      .asserting(namesOf(_) shouldBe (Set("Con"), Set("Con")))
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
      .asserting(namesOf(_) shouldBe (Set.empty, Set.empty))
  }

  // --- a pinned *return* is a declared capture: the body's row lands in the returned stack (capture legality). ---

  it should "count a pinned return's entries as declared (the body's row is captured into the stack)" in {
    val source = dischargePrelude + "def make[G[_]]: {X | G} Str = boom"
    rowCheck(source, Seq("make"), "make")
      .asserting(r => (r.derived.map(_.abilityName), r.leak) shouldBe (Set("X"), Set.empty))
  }

  // --- suspension is elaboration-only: a declared-suspended slot (open row on a by-value parameter) derives
  // exactly as a strict slot. ---

  it should "derive a declared-suspended slot identically to a strict slot (suspension is row-neutral)" in {
    val source = dischargePrelude +
      """def pureStr: Str
        |def branchStrict[A](c: Str, t: A, f: A): A = t
        |def branchSusp[A](c: Str, t: {X} A, f: {X} A): {X} A
        |def useStrict: {X} Str = branchStrict(pureStr, boom, boom)
        |def useSusp: {X} Str = branchSusp(pureStr, boom, boom)
        |""".stripMargin
    val names  = Seq("pureStr", "branchStrict", "branchSusp", "useStrict", "useSusp")
    (rowCheck(source, names, "useStrict"), rowCheck(source, names, "useSusp"))
      .mapN((strict, susp) => (namesOf(strict), namesOf(susp)))
      .asserting { case (strict, susp) => (strict, susp) shouldBe ((Set("X"), Set("X")), strict) }
  }

  // --- a suspended parameter's row counts inside the callee's own body wherever the computation is placed. ---

  it should "contribute a suspended parameter's declared row where the body places it" in {
    val source = dischargePrelude +
      "def if2[A](c: Str, value: {X} A): {X} A\ndef sel[A](a: A): A = a\ndef both[A](c: Str, v: {X} A): {X} A = sel(v)"
    rowCheck(source, Seq("if2", "sel", "both"), "both")
      .asserting(namesOf(_) shouldBe (Set("X"), Set("X")))
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
        (loop.leak, loop.derived.map(_.abilityName), bad.leak.map(_.abilityName)) shouldBe
          (Set.empty, Set("Nf", "Con"), Set("Nf"))
      }
  }

  // --- block/`val`: the applied-lambda desugar sequences the binder's effects into the row. ---

  it should "join a val-bound effectful binder's row through the block desugar" in {
    val source = prelude + "def logged: {Con} Str = {\nval x = readLine\nprintLine(x)\n}"
    rowCheck(source, Seq("items", "use", "logged"), "logged")
      .asserting(namesOf(_) shouldBe (Set("Con"), Set("Con")))
  }

  // --- a first-order ability (no higher-kinded carrier binder) is not an effect: its methods contribute nothing.
  // Requires the method's signature in the universe, so the ability-qualified name is fetched too. ---

  it should "not count a first-order ability method as an effect" in {
    val source = prelude + "ability Sh[A] { def sh(a: A): Str }\ndef shown: Str = sh(items)"
    rowCheck(source, Seq("items", "use", "shown"), "shown", abilityMethods = Seq(("sh", "Sh")))
      .asserting(r => (namesOf(r), r.unknownCallees) shouldBe ((Set("Con"), Set.empty), Set.empty))
  }

  // --- a platform run boundary (jvm's runMain shape) captures the whole argument row — tag source (ii). ---

  it should "clear the whole row at a registered run-boundary slot (the synthetic-main shape)" in {
    val source = prelude + "data IOish[A]\ndef runIt[A](io: IOish[A]): A\ndef entry: Str = runIt(items)"
    rowCheckWith(source, Seq("runIt", "entry", "items", "use"), "entry", runBoundaries = Set(vfqn("runIt")))
      .asserting(namesOf(_) shouldBe (Set.empty, Set.empty))
  }

  // --- a return headed by the platform run carrier (read off the boundary's own parameter, `main: IO[Unit]`) is the
  // nominal-run spelling: the whole derived row is captured by the concrete carrier. ---

  it should "capture the whole row at a run-carrier-headed return (main: IO[Unit] shape)" in {
    val source = prelude + "data IOish[A]\ndef runIt[A](io: IOish[A]): A\ndef entry: IOish[Str] = items"
    rowCheckWith(source, Seq("runIt", "entry", "items", "use"), "entry", runBoundaries = Set(vfqn("runIt")))
      .asserting(r => (r.derived.map(_.abilityName), r.leak) shouldBe (Set("Con"), Set.empty))
  }

  private def namesOf(result: RowResult): (Set[String], Set[String]) =
    (result.derived.map(_.abilityName), result.declared.map(_.abilityName))

  private def vfqn(name: String): ValueFQN = ValueFQN(testModule, QualifiedName(name, Qualifier.Default))

  private def rowCheck(
      source: String,
      names: Seq[String],
      target: String,
      abilityMethods: Seq[(String, String)] = Seq.empty
  ): IO[RowResult] =
    rowCheckWith(source, names, target, Set.empty, abilityMethods)

  /** Compile `source` through the real pipeline, collect the [[OperatorResolvedValue]]s of `names` (plus any
    * ability-qualified methods), and row-check `target` against that universe.
    */
  private def rowCheckWith(
      source: String,
      names: Seq[String],
      target: String,
      runBoundaries: Set[ValueFQN],
      abilityMethods: Seq[(String, String)] = Seq.empty
  ): IO[RowResult] =
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
      keys       = names.map(n => vfqn(n)) ++ abilityMethods.map { case (m, a) =>
                     ValueFQN(testModule, QualifiedName(m, Qualifier.Ability(a)))
                   }
      orvs      <- keys.traverse(k => generator.getFact(OperatorResolvedValue.Key(k)))
      errors    <- generator.currentErrors()
    } yield {
      if (errors.nonEmpty) throw new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}")
      val universe = Universe(orvs.flatten.map(orv => orv.vfqn -> orv).toMap, runBoundaries)
      RowChecker
        .checkValue(vfqn(target), universe)
        .getOrElse(throw new Exception(s"No OperatorResolvedValue for '$target'"))
    }
}
