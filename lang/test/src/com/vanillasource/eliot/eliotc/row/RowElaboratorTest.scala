package com.vanillasource.eliot.eliotc.row

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.compiler.IncrementalFactGenerator
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName as ModuleName2, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.nio.file.Path

/** The elaboration acceptance suite (docs/effects-as-rows.md §3): each case elaborates a **direct-style** definition
  * with [[RowElaborator]] and compares it *structurally* (α-renamed binders, positions ignored) against the
  * hand-written **explicit monadic twin** of the same program, compiled through the same pipeline. Structural equality
  * with the twin is the definition of correctness: the elaborator produces exactly the code a careful user would write
  * by hand.
  *
  * This is where a placement rule is pinned, since the twin says *what code the rule must produce* — an assertion no
  * behavioural test can make. Written as the R4 acceptance suite while the v2 checker still elaborated in parallel;
  * the twins outlived it unchanged, which is the point of comparing against source rather than against a mechanism.
  */
class RowElaboratorTest
    extends ProcessorTest(LangProcessors(systemModules = Seq(ModuleName2.systemFunctionModuleName))*) {
  private val testModule = ModuleName2(Seq.empty, "Test")

  /** The `eliot.carrier.Effect` machinery stub — hand-monadic twins name `flatMap`/`pure` from it, resolving to the
    * same FQNs ([[com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.effectFlatMapFQN]]) the elaborator mints.
    */
  private val effectStub =
    "ability Effect[F[_]] { def flatMap[A, B](f: A => F[B], fa: F[A]): F[B]\n" +
      "def pure[A](a: A): F[A]\ndef map[A, B](f: A => B, fa: F[A]): F[B] }"

  /** The `eliot.lang.Id` identity-carrier stub — pure-residual discharge twins name `runId`, resolving to the same
    * FQN ([[com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.runIdFQN]]) the elaborator mints at the
    * boundary (Appendix A.4).
    */
  private val idStub = "type Id[A]\ndef runId[A](obj: Id[A]): A"

  private val prelude =
    """import eliot.carrier.Effect
      |import eliot.lang.Id
      |data Str
      |ability Con[F[_]] { def readLine: {Con} Str
      |def printLine(s: Str): {Con} Str }
      |def use(s: Str): Str = s
      |def concat2(a: Str, b: Str): Str = a
      |def pureStr: Str
      |def strA: Str
      |def strB: Str
      |""".stripMargin

  /** The discharge environment: an effect ability `X` with its carrier and a catch-shaped discharger. */
  // The stub discharger mirrors the real `catch`/`else` spelling, and must keep mirroring it: its handler slot is
  // declared as an effect **row** (`Str => {Effect} A`), which is what makes a *pure* handler body lift (§1 rule 2).
  // Spelled `handler: Str => G[A]` it would mean "a computation on G" and a pure body would be a type error — the
  // shape these twins are not about. The `{Effect}` here denotes `G` itself, because the signature already binds one
  // `Effect`-constrained carrier (EffectSugarDesugarer's reuse rule).
  private val dischargePrelude =
    """data XCarrier[G, A]
      |ability X[F[_]] { def boom[A]: {X} A }
      |def catchX[G[_] ~ Effect, A](computation: {X | G} A, handler: Str => {Effect} A): G[A]
      |def failing: {X} Str = boom
      |""".stripMargin

  private val names          = Seq("use", "concat2", "pureStr", "strA", "strB")
  private val dischargeNames = Seq("catchX", "failing")

  "the row elaborator" should "hoist an effectful argument at a strict slot into a flatMap" in {
    compareToTwin(
      "def d: {Con} Str = printLine(readLine)",
      "def t: {Con} Str = flatMap(s -> printLine(s), readLine)"
    )
  }

  it should "turn an effectful val binding into a flatMap (block sequencing)" in {
    compareToTwin(
      "def d: {Con} Str = {\nval s = readLine\nprintLine(s)\n}",
      "def t: {Con} Str = flatMap(s -> printLine(s), readLine)"
    )
  }

  it should "sequence bare effectful statements with flatMap" in {
    compareToTwin(
      "def d: {Con} Str = {\nprintLine(strA)\nprintLine(strB)\n}",
      "def t: {Con} Str = flatMap(ignored -> printLine(strB), printLine(strA))"
    )
  }

  it should "pure-wrap a pure continuation tail after a bind" in {
    compareToTwin(
      "def d: {Con} Str = {\nval s = readLine\nuse(s)\n}",
      "def t: {Con} Str = flatMap(s -> pure(use(s)), readLine)"
    )
  }

  it should "pure-wrap a pure body under a declared row (the boundary lift)" in {
    compareToTwin(
      "def d: {Con} Str = pureStr",
      "def t: {Con} Str = pure(pureStr)"
    )
  }

  it should "nest two effectful arguments leftmost-outermost with a pure-wrapped core" in {
    compareToTwin(
      "def d: {Con} Str = concat2(readLine, readLine)",
      "def t: {Con} Str = flatMap(a -> flatMap(b -> pure(concat2(a, b)), readLine), readLine)"
    )
  }

  it should "hoist only the effectful argument of a mixed-argument call" in {
    compareToTwin(
      "def d: {Con} Str = concat2(pureStr, readLine)",
      "def t: {Con} Str = flatMap(r -> pure(concat2(pureStr, r)), readLine)"
    )
  }

  it should "keep a pure val binding an applied lambda (no bind, no pure)" in {
    compareToTwin(
      "def d: {Con} Str = {\nval x = pureStr\nprintLine(x)\n}",
      "def t: {Con} Str = {\nval x = pureStr\nprintLine(x)\n}"
    )
  }

  it should "leave a fully pure definition byte-identical (no Id, nothing inserted)" in {
    elaborated(prelude + "def d: Str = use(pureStr)", "d").asserting { case (elab, original) =>
      canonical(elab) shouldBe canonical(original)
    }
  }

  // --- suspended slots: an effectful argument passes unrun; a pure argument lifts into the carrier. The slot's
  // declared row is what decides it (§1 rule 2), never the argument's inferred type. ---

  it should "pure-wrap a pure argument at a declared-suspended slot and pass an effectful one unrun" in {
    val branchy = "def branch[A](c: Str, t: {Con} A, f: {Con} A): {Con} A\n"
    compareToTwin(
      branchy + "def d: {Con} Str = branch(strA, pureStr, readLine)",
      branchy + "def t: {Con} Str = branch(strA, pure(pureStr), readLine)",
      extraNames = Seq("branch")
    )
  }

  // --- discharge regions (Appendix A.4): a discharger call is carrier-valued by its declared return; under an empty
  // residual row its base carrier is Id, unwrapped with runId at that same boundary; under an ambient carrier it
  // rides like any effectful call. Handler lambdas at carrier-codomain slots elaborate as carrier regions. ---

  it should "unwrap a pure-residual discharge with runId and pure-wrap the pure handler body" in {
    compareToTwin(
      dischargePrelude + "def d: Str = catchX(failing, s -> pureStr)",
      dischargePrelude + "def t: Str = runId(catchX(failing, s -> pure(pureStr)))",
      extraNames = dischargeNames
    )
  }

  it should "leave a discharge with an effectful handler riding the ambient carrier untouched" in {
    compareToTwin(
      dischargePrelude + "def d: {Con} Str = catchX(failing, s -> printLine(s))",
      dischargePrelude + "def t: {Con} Str = catchX(failing, s -> printLine(s))",
      extraNames = dischargeNames
    )
  }

  it should "bind a val-bound discharge under an ambient carrier via flatMap" in {
    compareToTwin(
      dischargePrelude + "def d: {Con} Str = {\nval x = catchX(failing, s -> pureStr)\nprintLine(x)\n}",
      dischargePrelude + "def t: {Con} Str = flatMap(x -> printLine(x), catchX(failing, s -> pure(pureStr)))",
      extraNames = dischargeNames
    )
  }

  it should "unwrap a val-bound discharge with runId when the region has no ambient carrier" in {
    compareToTwin(
      dischargePrelude + "def d: Str = {\nval x = catchX(failing, s -> pureStr)\nuse(x)\n}",
      dischargePrelude + "def t: Str = {\nval x = runId(catchX(failing, s -> pure(pureStr)))\nuse(x)\n}",
      extraNames = dischargeNames
    )
  }

  it should "pass a discharge at a strict argument slot through unchanged when the region has no carrier" in {
    // No runId at argument slots — `Id` is written only at the two pure boundaries (a return and a `val` binding);
    // the argument's still-flex base must flow to the slot's expected type (the hand-monadic `runId(runAbort(x))`
    // shape, which would double-unwrap if the elaborator projected here too).
    compareToTwin(
      dischargePrelude + "def d: Str = use(catchX(failing, s -> pureStr))",
      dischargePrelude + "def t: Str = use(catchX(failing, s -> pure(pureStr)))",
      extraNames = dischargeNames
    )
  }

  it should "hoist a discharge at a strict argument slot under an ambient carrier via flatMap" in {
    compareToTwin(
      dischargePrelude + "def d: {Con} Str = printLine(catchX(failing, s -> pureStr))",
      dischargePrelude + "def t: {Con} Str = flatMap(x -> printLine(x), catchX(failing, s -> pure(pureStr)))",
      extraNames = dischargeNames
    )
  }

  it should "forward a suspended parameter as a carrier value without re-wrapping" in {
    compareToTwin(
      "def d(v: {Con} Str): {Con} Str = v",
      "def t(v: {Con} Str): {Con} Str = v"
    )
  }

  // --- callback lambdas: an `{Effect}`-marker arrow slot (`action: A => {Effect} B` — codomain on the callee's
  // minted carrier) is a carrier-codomain slot, NOT a suspended slot: the lambda passes as a value, its body is a
  // carrier region. A lambda at a plain arrow slot elaborates naturally; a carrier-valued body instantiates a
  // bare-generic codomain at a carrier (the generic-eliminator rule). ---

  private val eachPrelude = "def each(s: Str, action: Str => {Effect} Str): {Effect} Str\n"

  it should "pass an effectful lambda at an Effect-marker callback slot untouched (not suspended, not pure-wrapped)" in {
    compareToTwin(
      eachPrelude + "def d: {Con} Str = each(strA, x -> printLine(x))",
      eachPrelude + "def t: {Con} Str = each(strA, x -> printLine(x))",
      extraNames = Seq("each")
    )
  }

  // A pure callback leaves the callee's declared row **empty**, and the empty row's value is `Id` (§1 rule 4): the
  // call runs at `Id` and is projected straight back with `runId`, whatever carrier the region has. The body still
  // `pure`-wraps — the slot declares a computation — and the whole `Id` round trip erases before codegen.
  it should "pure-wrap the body of a pure lambda at an Effect-marker callback slot, at the empty row" in {
    compareToTwin(
      eachPrelude + "def d: {Con} Str = each(strA, x -> use(x))",
      eachPrelude + "def t: {Con} Str = runId(each(strA, x -> pure(use(x))))",
      extraNames = Seq("each")
    )
  }

  it should "elaborate a block-bodied lambda at a plain arrow slot naturally (generic eliminator)" in {
    val weird = "def weird[A, B](f: A => B, a: A): B\n"
    compareToTwin(
      weird + "def d: {Con} Str = weird(x -> {\nval y = readLine\nprintLine(y)\n}, strA)",
      weird + "def t: {Con} Str = weird(x -> flatMap(y -> printLine(y), readLine), strA)",
      extraNames = Seq("weird")
    )
  }

  it should "treat a generic-eliminator call as carrier-valued when the lambda instantiates its return at a carrier" in {
    val weird = "def weird[A, B](f: A => B, a: A): B\n"
    compareToTwin(
      weird + "def d: {Con} Str = weird(x -> printLine(x), strA)",
      weird + "def t: {Con} Str = weird(x -> printLine(x), strA)",
      extraNames = Seq("weird")
    )
  }

  it should "lift a generic-headed return its arguments instantiate at a payload (§1 rule 4)" in {
    // `weird`'s declared return is the bare generic `B`, which `f: A => B` determines: the lambda's body is the pure
    // `use(x)`, so `B` is a payload and the call is a plain value the return boundary `pure`-wraps. (This asserted
    // the A.8.6 spelling — write nothing, let the checker finish the node from the solved instantiation — which is
    // the deferral rule 4 removes: a plain generic is a payload, always.)
    val weird = "def weird[A, B](f: A => B, a: A): B\n"
    compareToTwin(
      weird + "def d: {Con} Str = weird(x -> use(x), strA)",
      weird + "def t: {Con} Str = pure(weird(x -> use(x), strA))",
      extraNames = Seq("weird")
    )
  }

  it should "classify a hoisted binder at an arrow slot from the expression whose payload it holds" in {
    // The argument at `f: A => B` is a *function value* whose construction performs, so rule 1 hoists it into the
    // enclosing region and the slot receives the resulting binder. Classifying that binder needs its kind after one
    // *further* application — which a declared parameter's type would give and a minted binder has none of. Reading it
    // instead off the expression the binder holds the payload of (`concat2(readLine)`, a payload by its own
    // declaration) is what keeps `B` a payload here; left unknown, the core is not `pure`-wrapped and the checker
    // solves the rowless `B` at a computation to make the inserted `flatMap` fit.
    val weird = "def weird[A, B](f: A => B, a: A): B\n"
    compareToTwin(
      weird + "def d: {Con} Str = weird(concat2(readLine), strA)",
      weird + "def t: {Con} Str = flatMap(g -> pure(weird(g, strA)), flatMap(r -> pure(concat2(r)), readLine))",
      extraNames = Seq("weird")
    )
  }

  it should "elaborate a callback-calling body on the callee side (applied function-typed parameter)" in {
    compareToTwin(
      "def d(s: Str, action: Str => {Effect} Str): {Effect} Str = action(s)",
      "def t(s: Str, action: Str => {Effect} Str): {Effect} Str = action(s)"
    )
  }

  it should "hoist an effectful argument of an applied function-typed parameter" in {
    compareToTwin(
      "def d(action: Str => {Effect} Str): {Con} Str = action(readLine)",
      "def t(action: Str => {Effect} Str): {Con} Str = flatMap(x -> action(x), readLine)"
    )
  }

  // --- pinned/run-boundary captures are their own carrier regions: a compound captured computation binds on the
  // pinned/run stack even under a pure definition, and a pure captured argument lifts via pure. A pinned *return*
  // makes the whole body a carrier region; a nominal-run return (IOish[Str]) likewise. ---

  it should "elaborate a compound pinned argument as its own carrier region (binds on the pinned stack)" in {
    compareToTwin(
      dischargePrelude + "def d: Str = catchX(use(boom), s -> pureStr)",
      dischargePrelude + "def t: Str = runId(catchX(flatMap(x -> pure(use(x)), boom), s -> pure(pureStr)))",
      extraNames = dischargeNames
    )
  }

  it should "leave a pure argument at a pinned slot unwrapped (pinned means captured)" in {
    // Pinned captures take the computation as-is: a pure actual is not lifted — the val-bound-discharge limitation's
    // "Expected: {Abort | IO} String" mismatch is by design, and a boundary `pure` would silently replace that
    // curated diagnostic with a downstream ability demand.
    compareToTwin(
      dischargePrelude + "def d: Str = catchX(pureStr, s -> pureStr)",
      dischargePrelude + "def t: Str = runId(catchX(pureStr, s -> pure(pureStr)))",
      extraNames = dischargeNames
    )
  }

  it should "pure-wrap a pure body under a pinned return (the body is the captured computation)" in {
    compareToTwin(
      dischargePrelude + "def d[G[_]]: {X | G} Str = pureStr",
      dischargePrelude + "def t[G[_]]: {X | G} Str = pure(pureStr)",
      extraNames = dischargeNames
    )
  }

  it should "leave an effectful body under a pinned return untouched" in {
    compareToTwin(
      dischargePrelude + "def d[G[_]]: {X | G} Str = boom",
      dischargePrelude + "def t[G[_]]: {X | G} Str = boom",
      extraNames = dischargeNames
    )
  }

  // --- a parameter is classified by its *declared type*, applied or not (`Combine[List[A]]`'s own `combine`). The
  // atomic-type-only reading left `a: Lst[A]` unclassified, so neither half of the empty-row rewrite fired: the call
  // did not settle `ρ := {}` and `a` was not lifted, while the pure boundary still wrote `runId` — leaving the
  // checker a bare `Lst[A]` against the `Id[Lst[A]]` that same `runId` implies. ---

  private val foldPrelude =
    "type Lst[A]\n" +
      "def app[A](list: Lst[A], element: A): Lst[A]\n" +
      "def foldL[A, B](initial: {Effect} B, combine: A => B => {Effect} B, list: Lst[A]): {Effect} B\n"

  it should "write an applied-type parameter's fold at the empty row, lifting it with pure under runId" in {
    compareToTwin(
      foldPrelude + "def d[A](a: Lst[A], b: Lst[A]): Lst[A] = foldL(a, e -> acc -> app(acc, e), b)",
      foldPrelude + "def t[A](a: Lst[A], b: Lst[A]): Lst[A] = runId(foldL(pure(a), e -> acc -> pure(app(acc, e)), b))",
      extraNames = Seq("app", "foldL")
    )
  }

  it should "keep a concrete-carrier parameter a computation, not a plain value at a concrete-carrier slot" in {
    // The dual, and why the classification reads a *concrete carrier head* rather than "the declared type is atomic":
    // `Id[A]` is a carrier however it is spelled, so `runId`'s own `obj` is the computation the accessor takes apart.
    // Read as a plain value it is offered a `pure`, reported as the rule-2 violation below — inside the jvm layer's
    // own `Id.els`, a file the user never wrote.
    violationsOf(prelude + "def hold[A](obj: Id[A]): A\ndef d[A](obj: Id[A]): A = hold(obj)", "d", Seq("hold"))
      .asserting(_ shouldBe Seq.empty)
  }

  private val runPrelude = "data IOish[A]\ndef runIt[A](io: IOish[A]): A\n"

  it should "treat a nominal-run return as a carrier region (main: IO[Unit] shape, no runId)" in {
    compareToTwin(
      runPrelude + "def d: IOish[Str] = readLine",
      runPrelude + "def t: IOish[Str] = readLine",
      extraNames = Seq("runIt"),
      runBoundaries = Set("runIt")
    )
  }

  it should "elaborate a compound run-boundary argument on the run carrier under a pure definition" in {
    compareToTwin(
      runPrelude + "def d: Str = runIt({\nval x = readLine\nprintLine(x)\n})",
      runPrelude + "def t: Str = runIt(flatMap(x -> printLine(x), readLine))",
      extraNames = Seq("runIt"),
      runBoundaries = Set("runIt")
    )
  }

  /** Elaborate `d` from `direct` and structurally compare with `t`'s compiled runtime from `twin`. */
  private def compareToTwin(
      direct: String,
      twin: String,
      extraNames: Seq[String] = Seq.empty,
      runBoundaries: Set[String] = Set.empty
  ): IO[org.scalatest.Assertion] =
    for {
      d      <- elaborated(prelude + direct, "d", extraNames, runBoundaries)
      t      <- runtimeOf(prelude + twin, "t", extraNames, runBoundaries)
    } yield canonical(d._1) shouldBe canonical(t)

  /** The elaborated body of `name` plus its original runtime (both as expressions). */
  private def elaborated(
      source: String,
      name: String,
      extraNames: Seq[String] = Seq.empty,
      runBoundaries: Set[String] = Set.empty
  ): IO[(OperatorResolvedExpression, OperatorResolvedExpression)] =
    universeOf(source, name, extraNames, runBoundaries).map { universe =>
      val orv = universe.values(vfqn(name))
      val el  = RowElaborator
        .elaborate(orv, universe)
        .getOrElse(throw new Exception(s"No runtime for '$name'"))
      (el.value, orv.runtime.get.value)
    }

  /** The rule-2/rule-4 violations elaborating `name` records — the diagnostics the processor turns into errors. */
  private def violationsOf(source: String, name: String, extraNames: Seq[String]): IO[Seq[String]] =
    universeOf(source, name, extraNames).map { universe =>
      RowElaborator.elaborateChecked(universe.values(vfqn(name)), universe).violations.map(_.message.value)
    }

  private def runtimeOf(
      source: String,
      name: String,
      extraNames: Seq[String],
      runBoundaries: Set[String] = Set.empty
  ): IO[OperatorResolvedExpression] =
    universeOf(source, name, extraNames, runBoundaries).map(_.values(vfqn(name)).runtime.get.value)

  /** Canonical structural rendering: binders α-renamed in traversal order, positions and type arguments ignored. */
  private def canonical(expr: OperatorResolvedExpression): String = {
    import OperatorResolvedExpression.*
    var counter = 0
    def go(e: OperatorResolvedExpression, env: Map[String, String]): String = e match {
      case FunctionApplication(t, a)  => s"${go(t.value, env)}(${go(a.value, env)})"
      case FunctionLiteral(p, _, b)   =>
        val nm = s"%$counter"
        counter += 1
        s"($nm -> ${go(b.value, env + (p.value -> nm))})"
      case ParameterReference(n)      => env.getOrElse(n.value, n.value)
      case ValueReference(n, _)       => n.value.toString
      case IntegerLiteral(v)          => v.value.toString
      case StringLiteral(v)           => s"\"${v.value}\""
    }
    go(expr, Map.empty)
  }

  private def vfqn(name: String): ValueFQN = ValueFQN(testModule, QualifiedName(name, Qualifier.Default))

  private def universeOf(
      source: String,
      target: String,
      extraNames: Seq[String] = Seq.empty,
      runBoundaries: Set[String] = Set.empty
  ): IO[RowChecker.Universe] =
    for {
      generator <- IncrementalFactGenerator.create(SequentialCompilerProcessors(processors), None)
      _         <- generator.registerFact(SourceContent(file, Sourced(file, PositionRange.zero, source)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file), Platform.Compiler))
      imports    = systemImports :+ SystemImport("Effect", effectStub, Seq("eliot", "carrier")) :+
                     SystemImport("Id", idStub)
      _         <- imports.traverse { imp =>
                     val modulePath = imp.moduleName.toPath
                     val impFile    = java.net.URI.create(modulePath.toString)
                     generator.registerFact(PathScan(modulePath, Seq(impFile))) >>
                       generator.registerFact(PathScan(modulePath, Seq(impFile), Platform.Compiler)) >>
                       generator.registerFact(SourceContent(impFile, Sourced(impFile, PositionRange.zero, imp.content)))
                   }
      // The `=>` alias's own definition is part of the universe so the elaborator can see through it to the arrow
      // (`handler: Str => G[A]` reaches the operator phase as the unexpanded alias application; an operator-named
      // alias lives in the Default namespace, exactly like the equivalent `def`).
      arrowAlias = ValueFQN(ModuleName2.systemFunctionModuleName, QualifiedName("=>", Qualifier.Default))
      // The prelude abilities' own methods. An ability method contributes its *declared row* like any other callee
      // (it is no longer a special case that reads its ability off the qualifier), so it has to be looked up like any
      // other callee too — absent from the universe it derives nothing and nothing is hoisted around it.
      keys       = (names ++ extraNames :+ target).map(vfqn) :+ arrowAlias :++
                     Seq(("readLine", "Con"), ("printLine", "Con"), ("boom", "X"))
                       .filter { case (method, _) => source.contains(s"def $method") }
                       .map { case (method, ability) =>
                         ValueFQN(testModule, QualifiedName(method, Qualifier.Ability(ability)))
                       }
      orvs      <- keys.traverse(k => generator.getFact(OperatorResolvedValue.Key(k)))
      errors    <- generator.currentErrors()
    } yield {
      if (errors.nonEmpty) throw new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}")
      RowChecker.Universe(orvs.flatten.map(orv => orv.vfqn -> orv).toMap, runBoundaries.map(vfqn))
    }
}
