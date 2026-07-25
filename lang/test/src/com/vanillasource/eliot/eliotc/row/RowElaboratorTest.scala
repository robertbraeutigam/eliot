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

/** The R4 first-slice acceptance suite (docs/effects-as-rows.md §3/§8): each case elaborates a **direct-style**
  * definition with [[RowElaborator]] and compares it *structurally* (α-renamed binders, positions ignored) against the
  * hand-written **explicit monadic twin** of the same program, compiled through the same pipeline. Structural equality
  * with the twin is the slice's definition of correctness: the elaborator produces exactly the code a careful user
  * writes by hand today — which is also the shape the v2 checker's own elaboration converges to.
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

  private val prelude =
    """import eliot.carrier.Effect
      |data Str
      |ability Con[F[_]] { def readLine: F[Str]
      |def printLine(s: Str): F[Str] }
      |def use(s: Str): Str = s
      |def concat2(a: Str, b: Str): Str = a
      |def pureStr: Str
      |def strA: Str
      |def strB: Str
      |""".stripMargin

  private val names = Seq("use", "concat2", "pureStr", "strA", "strB")

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

  // --- suspended slots: an effectful argument passes unrun; a pure argument lifts into the carrier — v2's
  // pure-wrap arm, now a declared-slot-mode read. ---

  it should "pure-wrap a pure argument at a declared-suspended slot and pass an effectful one unrun" in {
    val branchy = "def branch[A](c: Str, t: {Con} A, f: {Con} A): {Con} A\n"
    compareToTwin(
      branchy + "def d: {Con} Str = branch(strA, pureStr, readLine)",
      branchy + "def t: {Con} Str = branch(strA, pure(pureStr), readLine)",
      extraNames = Seq("branch")
    )
  }

  /** Elaborate `d` from `direct` and structurally compare with `t`'s compiled runtime from `twin`. */
  private def compareToTwin(direct: String, twin: String, extraNames: Seq[String] = Seq.empty): IO[org.scalatest.Assertion] =
    for {
      d      <- elaborated(prelude + direct, "d", extraNames)
      t      <- runtimeOf(prelude + twin, "t", extraNames)
    } yield canonical(d._1) shouldBe canonical(t)

  /** The elaborated body of `name` plus its original runtime (both as expressions). */
  private def elaborated(
      source: String,
      name: String,
      extraNames: Seq[String] = Seq.empty
  ): IO[(OperatorResolvedExpression, OperatorResolvedExpression)] =
    universeOf(source, name, extraNames).map { universe =>
      val orv = universe.values(vfqn(name))
      val el  = RowElaborator
        .elaborate(orv, universe)
        .getOrElse(throw new Exception(s"No runtime for '$name'"))
      (el.value, orv.runtime.get.value)
    }

  private def runtimeOf(source: String, name: String, extraNames: Seq[String]): IO[OperatorResolvedExpression] =
    universeOf(source, name, extraNames).map(_.values(vfqn(name)).runtime.get.value)

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

  private def universeOf(source: String, target: String, extraNames: Seq[String] = Seq.empty): IO[RowChecker.Universe] =
    for {
      generator <- IncrementalFactGenerator.create(SequentialCompilerProcessors(processors), None)
      _         <- generator.registerFact(SourceContent(file, Sourced(file, PositionRange.zero, source)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file), Platform.Compiler))
      imports    = systemImports :+ SystemImport("Effect", effectStub, Seq("eliot", "carrier"))
      _         <- imports.traverse { imp =>
                     val modulePath = imp.moduleName.toPath
                     val impFile    = java.net.URI.create(modulePath.toString)
                     generator.registerFact(PathScan(modulePath, Seq(impFile))) >>
                       generator.registerFact(PathScan(modulePath, Seq(impFile), Platform.Compiler)) >>
                       generator.registerFact(SourceContent(impFile, Sourced(impFile, PositionRange.zero, imp.content)))
                   }
      keys       = (names ++ extraNames :+ target).map(vfqn)
      orvs      <- keys.traverse(k => generator.getFact(OperatorResolvedValue.Key(k)))
      errors    <- generator.currentErrors()
    } yield {
      if (errors.nonEmpty) throw new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}")
      RowChecker.Universe(orvs.flatten.map(orv => orv.vfqn -> orv).toMap)
    }
}
