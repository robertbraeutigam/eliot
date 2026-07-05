package com.vanillasource.eliot.eliotc.ability

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** Ability-implementation guards Stage 2: the **use-site discharge** of a `where` guard and its 3-way interpretation.
  *
  * A guard rides the marker's return-type slot (§2.3). When [[com.vanillasource.eliot.eliotc.ability.util.AbilityMatcher]]
  * matches an impl at a concrete type-argument tuple, `AbilityImplementationProcessor` discharges the guard by
  * monomorphizing the marker on the **compiler track** (guards are compile-time computations) and reading its reduced
  * return value:
  *   - a `Bool` `true` (or a `Right(true)`) keeps the candidate — the instance resolves;
  *   - a `Bool` `false` (or a `Right(false)`) declines — the candidate is dropped, so with no other instance the
  *     resolution reports "No ability implementation found";
  *   - a `Left(msg)` is a hard compile error carrying the author's message.
  *
  * An unguarded impl (the default `eliot.lang.Bool::true` return slot) keeps verbatim, without monomorphizing the
  * marker at all — the pre-guards behaviour.
  *
  * The whole scenario lives in the **compiler** source pool (self-contained: `Bool` + `Either` + `Function`, no effect
  * machinery), so the guards reduce through constructor / native reductions alone. The guards are hand-built here — the
  * `error`/`E1 != E2` combinators that produce these verdicts in real code are exercised by their own tests
  * (`CompilerAbilityResolutionTest`, `GuardSignatureIntegrationTest`, and Stage 4's `Throw` client).
  */
class AbilityGuardDischargeTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {

  private def compilerScan(pkg: Seq[String], name: String, content: String): Seq[SourceContent | PathScan] = {
    val path = (pkg :+ s"$name.els").foldLeft(Path.of(""))(_ `resolve` _)
    val uri  = URI.create((pkg :+ s"$name.els").mkString("/"))
    Seq(
      PathScan(path, Seq(uri), Platform.Compiler),
      SourceContent(uri, Sourced(uri, PositionRange.zero, content))
    )
  }

  // The self-contained base every guard needs: `Function` (arrow signatures), `Type`, `String`, the compile-time
  // `Bool` (its `true`/`false`/`fold` reductions come from `SystemNativesProcessor`), and the compile-time `Either`
  // carrier whose `Left`/`Right` constructors the discharge inspects.
  private val baseFacts: Seq[CompilerFact] =
    (compilerScan(Seq("eliot", "compiler"), "Type", "type Type") ++
      compilerScan(
        Seq("eliot", "lang"),
        "Function",
        "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"
      ) ++
      compilerScan(Seq("eliot", "lang"), "String", "type String") ++
      compilerScan(
        Seq("eliot", "lang"),
        "Bool",
        "import eliot.lang.Function\ntype Bool\ndef true: Bool\ndef false: Bool\ndef fold[A](cond: Bool, whenTrue: A, whenFalse: A): A"
      ) ++
      compilerScan(
        Seq("eliot", "lang"),
        "Either",
        "import eliot.lang.Function\ndata Either[E, A] = Left(error: E) | Right(value: A)"
      )).collect { case f: CompilerFact => f }

  private val moduleName = ModuleName(Seq("test"), "M")

  /** The `Show[Widget]` ability + type, plus an `implement Show[Widget]` carrying `guardClause` (a `where <expr>` or the
    * empty string for an unguarded instance). Everything colocated in one module so the implement sits with its ability.
    */
  private def moduleContent(guardClause: String): String =
    s"""import eliot.lang.Function
       |import eliot.lang.Bool
       |import eliot.lang.Either
       |
       |ability Show[A] { def show(x: A): A }
       |
       |data Widget
       |
       |implement Show[Widget] $guardClause {
       |   def show(x: Widget): Widget = x
       |}
       |""".stripMargin

  private def facts(guardClause: String): Seq[CompilerFact] =
    baseFacts ++ compilerScan(Seq("test"), "M", moduleContent(guardClause)).collect { case f: CompilerFact => f }

  private val showVfqn        = ValueFQN(moduleName, QualifiedName("show", Qualifier.Ability("Show")))
  private val widgetArg: GroundValue =
    GroundValue.Structure(ValueFQN(moduleName, QualifiedName("Widget", Qualifier.Type)), Seq.empty, GroundValue.Type)

  private def resolve(guardClause: String): IO[(Option[AbilityImplementation], Seq[TestError])] =
    runGeneratorWithFacts(facts(guardClause), AbilityImplementation.Key(showVfqn, Seq(widgetArg), Platform.Compiler))
      .map { case (impl, errors) => (impl, toTestErrors(errors)) }

  private def resolvedName(guardClause: String): IO[Option[String]] =
    resolve(guardClause).map { case (impl, _) => impl.map(_.implementationFQN.name.name) }

  "an unguarded implementation" should "resolve (the default `true` return slot, no discharge)" in {
    resolvedName("").asserting(_ shouldBe Some("show"))
  }

  "a guard reducing to `true`" should "keep the candidate — the instance resolves" in {
    resolvedName("where fold(false, false, true)").asserting(_ shouldBe Some("show"))
  }

  "a guard reducing to `false`" should "decline — the instance does not resolve" in {
    resolve("where false").asserting { case (impl, _) => impl shouldBe None }
  }

  it should "report no implementation once the sole candidate declines" in {
    resolve("where false").asserting { case (_, errors) =>
      errors.map(_.message).exists(_.contains("No ability implementation")) shouldBe true
    }
  }

  "a guard `Right(true)`" should "keep the candidate — the success payload is `true`" in {
    resolvedName("where Right(true)").asserting(_ shouldBe Some("show"))
  }

  "a guard `Right(false)`" should "decline — the success payload is `false`" in {
    resolve("where Right(false)").asserting { case (impl, _) => impl shouldBe None }
  }

  "a guard `Left(msg)`" should "be a hard compile error carrying the author's message" in {
    resolve("""where Left("Widget is not showable here")""").asserting { case (_, errors) =>
      errors.map(_.message).exists(_.contains("Widget is not showable here")) shouldBe true
    }
  }

  it should "produce no implementation fact when the guard rejects" in {
    resolve("""where Left("nope")""").asserting { case (impl, _) => impl shouldBe None }
  }
}
