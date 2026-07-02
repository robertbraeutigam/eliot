package com.vanillasource.eliot.eliotc

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.resolve.fact.{
  QualifiedName as ResolveQualifiedName,
  Qualifier as ResolveQualifier
}
import com.vanillasource.eliot.eliotc.compiler.IncrementalFactGenerator
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI
import java.nio.file.Path

abstract class ProcessorTest(val processors: CompilerProcessor*) extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  val file           = URI.create("Test.els")
  val testModuleName = ModuleName(Seq.empty, "Test")
  val sourceContent  = SourceContent(file, Sourced(file, PositionRange.zero, "test source"))
  /** The canonical module environment every test runs in: one stub per `ModuleName.defaultSystemModules` entry (so the
    * auto-imports resolve), plus modules that are *registered (loadable) but not auto-imported* — the import-required
    * effects `Console`/`Log`/`Dep` (in `effectPackage`; a snippet that prints must `import eliot.effect.Console`), and
    * the modules the resolver/ability-checker loads by *fixed FQN* rather than by import: the desugaring-machinery
    * `PatternMatch`/`TypeMatch` (in `compilerInternalPackage`) and `Type` (in `compilerPackage`, reached via the
    * resolver's bare-name special-case → `WellKnownTypes.typeFQN`). This is the SINGLE place the module set and each
    * module's package live. A test that needs richer content for a few modules calls [[ambientStubsWith]] to override
    * just those, instead of re-listing the whole set — so relocating a module, or adding/removing one, touches only this
    * list.
    */
  val systemImports  = Seq(
    SystemImport("Function", "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"),
    SystemImport("Type", "type Type", ModuleName.compilerPackage),
    SystemImport("BigInteger", "type BigInteger"),
    SystemImport("Unit", "type Unit"),
    SystemImport("String", "type String"),
    SystemImport("IO", "type IO"),
    SystemImport("PatternMatch", "", ModuleName.compilerInternalPackage),
    SystemImport("TypeMatch", "", ModuleName.compilerInternalPackage),
    SystemImport("Int", ProcessorTest.intStubContent),
    SystemImport("Runtime", ProcessorTest.runtimeStubContent),
    SystemImport("Console", ProcessorTest.consoleStubContent, ModuleName.effectPackage),
    SystemImport("Log", ProcessorTest.logStubContent, ModuleName.effectPackage),
    SystemImport("Dep", ProcessorTest.depStubContent, ModuleName.effectPackage)
  )

  /** The canonical [[systemImports]] with the named modules' content replaced — existing entries keep their package
    * (so e.g. `"PatternMatch" -> realDecl` stays in `compilerInternalPackage`), and any name not already present is
    * added as a new `eliot.lang` stub. Lets a test enrich only the modules it exercises (a richer `Int`, a real
    * `PatternMatch` ability, an extra `Bool`/`Option`) without restating the rest of the prelude.
    */
  def ambientStubsWith(overrides: (String, String)*): Seq[SystemImport] = {
    val overrideMap = overrides.toMap
    systemImports.map(s => overrideMap.get(s.module).fold(s)(c => s.copy(content = c))) ++
      overrides.collect { case (name, content) if !systemImports.exists(_.module == name) => SystemImport(name, content) }
  }

  def sourced[T](value: T): Sourced[T] = Sourced(file, PositionRange.zero, value)

  def default(name: String): QualifiedName = QualifiedName(name, Qualifier.Default)

  def toResolve(qn: QualifiedName): ResolveQualifiedName =
    ResolveQualifiedName(
      qn.name,
      qn.qualifier match {
        case Qualifier.Default    => ResolveQualifier.Default
        case Qualifier.Type       => ResolveQualifier.Type
        case Qualifier.Ability(n) => ResolveQualifier.Ability(n)
        case _                    => throw IllegalArgumentException("Cannot convert AbilityImplementation in test helper")
      }
    )

  def createGenerator(facts: Seq[CompilerFact]): IO[IncrementalFactGenerator] =
    for {
      generator <- IncrementalFactGenerator.create(SequentialCompilerProcessors(processors), None)
      _         <- generator.registerFact(sourceContent)
      _         <- facts.traverse_(generator.registerFact)
    } yield generator

  def runGeneratorWithFacts[K <: CompilerFact](
      facts: Seq[CompilerFact],
      trigger: CompilerFactKey[K]
  ): IO[(Option[K], Seq[CompilerError])] =
    for {
      generator <- createGenerator(facts)
      result    <- generator.getFact(trigger)
      errors    <- generator.currentErrors()
    } yield (result, errors)

  def runGenerator(
      source: String,
      trigger: CompilerFactKey[? <: CompilerFact],
      imports: Seq[SystemImport] = Seq.empty
  ): IO[(Seq[CompilerError], Map[CompilerFactKey[?], CompilerFact])] =
    for {
      generator <- IncrementalFactGenerator.create(SequentialCompilerProcessors(processors), None)
      _         <- generator.registerFact(SourceContent(file, Sourced(file, PositionRange.zero, source)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file)))
      _         <- imports.traverse { imp =>
                     val modulePath = imp.moduleName.toPath
                     val impFile    = URI.create(modulePath.toString)
                     generator.registerFact(PathScan(modulePath, Seq(impFile))) >>
                       generator.registerFact(SourceContent(impFile, Sourced(impFile, PositionRange.zero, imp.content)))
                   }
      _         <- generator.getFact(trigger)
      facts     <- generator.currentFacts()
      errors    <- generator.currentErrors()
    } yield (errors, facts)

  /** A source stub for a system module the harness pre-registers (its `.els` path is derived from
    * [[moduleName]] via the shared [[ModuleName.toPath]] layout). `packages` defaults to the `eliot.lang` prelude;
    * relocated modules (e.g. `PatternMatch`/`TypeMatch` in [[ModuleName.compilerInternalPackage]]) are declared once in
    * the canonical [[systemImports]], so tests reach them through [[ambientStubsWith]] and never repeat the package.
    */
  case class SystemImport(module: String, content: String, packages: Seq[String] = ModuleName.defaultSystemPackage) {
    def moduleName: ModuleName = ModuleName(packages, module)
  }

  case class TestError(message: String, highlight: String)

  extension (message: String) infix def at(highlight: String): TestError = TestError(message, highlight)

  def toTestErrors(errors: Seq[CompilerError]): Seq[TestError] =
    errors.map { error =>
      val lines     = error.content.linesIterator.toSeq
      val from      = error.sourceRange.from
      val to        = error.sourceRange.to
      val highlight =
        if (from.line == to.line) lines(from.line - 1).substring(from.col - 1, to.col - 1)
        else {
          val firstLine   = lines(from.line - 1).substring(from.col - 1)
          val lastLine    = lines(to.line - 1).substring(0, to.col - 1)
          val middleLines = lines.slice(from.line, to.line - 1)
          (firstLine +: middleLines :+ lastLine).mkString("\n")
        }
      TestError(error.message, highlight)
    }
}

object ProcessorTest {

  /** Declarations for the built-in opaque `Bool` type and its compile-time predicates, mirroring
    * `lang/eliot/eliot/lang/Bool.els`. The reductions are supplied by `SystemNativesProcessor`.
    */
  val boolImportContent: String =
    "type Bool\ndef true: Bool\ndef false: Bool\ninfix def &&(a: Bool, b: Bool): Bool"

  /** Minimal ambient `Int`/`Runtime` stubs. As of the Phase-6 literal desugar every value-position integer literal `n`
    * is rewritten to `integerLiteral[n] : Int[n, n]`, so `Int` and `Runtime` are in `defaultSystemModules` (always
    * auto-imported) and the test harness must register matching stubs. These minimal versions only declare the abstract
    * `Int` type and the `integerLiteral` constructor; the richer `Coerce`/`Combine`/arithmetic environment lives in the
    * `Int` tests' own import lists.
    */
  /** The package holding desugaring machinery relocated out of the `eliot.lang` prelude (`PatternMatch`/`TypeMatch`).
    * Re-exported here so the few tests that build a custom (non-`ambientStubsWith`) import set can register those stubs
    * at the FQN the resolver/ability-checker loads. Most tests never need it — `ambientStubsWith` preserves the package.
    */
  val compilerInternalPackage: Seq[String] = ModuleName.compilerInternalPackage

  /** The package holding the compiler-coordinated abilities the checker resolves by FQN — `Coerce` (check-mode
    * widening) and `Combine` (covariant join). Re-exported so the `Int` tests can register those stubs at the FQN
    * `WellKnownTypes` loads. Deliberately *not* in the bare ambient prelude: a type-mismatch test resolves `coerceFQN`,
    * which must find no module (clean mismatch) unless the test opts into the full `Int` environment.
    */
  val compilerPackage: Seq[String] = ModuleName.compilerPackage

  /** The `eliot.effect` package holding the effect surface + machinery. Re-exported so the few tests that build a
    * custom (non-`ambientStubsWith`) import set can register the ambient `Console`/`Log`/`Dep` stubs at their real FQN.
    * Most tests never need it — `ambientStubsWith` preserves each module's package automatically.
    */
  val effectPackage: Seq[String] = ModuleName.effectPackage

  val intStubContent: String     = "type Int[auto MIN: BigInteger, auto MAX: BigInteger]"
  val runtimeStubContent: String = "def integerLiteral[V: BigInteger]: Int[V, V]"

  /** The real `PatternMatch`/`TypeMatch` ability declarations (the canonical `systemImports` register them empty, since
    * they are loaded by FQN only when a snippet has a `data`/`match`). Tests exercising `match` or field access enrich
    * them via `ambientStubsWith("PatternMatch" -> patternMatchAbilityStub, "TypeMatch" -> typeMatchAbilityStub)`.
    */
  val patternMatchAbilityStub: String =
    "ability PatternMatch[T] {\ntype Cases[R]\ndef handleCases[R](value: T, cases: Cases[R]): R\n}"
  val typeMatchAbilityStub: String     =
    "ability TypeMatch[T] {\ntype Fields[R]\ndef typeMatch[R](value: Type, matched: Fields[R], notMatched: Function[Unit, R]): R\n}"

  /** `Console` effect stub, mirroring `stdlib/eliot/eliot/effect/Console.els`. Import-required (in `effectPackage`, not
    * ambient), registered so a snippet that does `import eliot.effect.Console` resolves. The concrete JVM instance lives
    * in the real jvm layer, not here.
    */
  val consoleStubContent: String =
    "ability Console[F[_]] {\ndef printLine(s: String): F[Unit]\ndef readLine: F[String]\n}"

  /** `Log` effect stub, mirroring `stdlib/eliot/eliot/effect/Log.els`. Import-required (in `effectPackage`); the
    * concrete JVM instance lives in the real jvm layer.
    */
  val logStubContent: String = "ability Log[F[_]] {\ndef log(s: String): F[Unit]\n}"

  /** `Dep` effect stub, mirroring `stdlib/eliot/eliot/effect/Dep.els`. Import-required (in `effectPackage`);
    * application/layer code supplies the concrete instances per dependency type.
    */
  val depStubContent: String = "ability Dep[X, F[_]] {\ndef dependency: F[X]\n}"

  /** The auto-imported system modules minus the Phase-6 ambient `Int`/`Runtime`. Tests that use `Int`/`integerLiteral`
    * as a *local* declaration name (and never write a value-position integer literal) pass this to
    * `ModuleValueProcessor` so the ambient versions do not shadow their local ones. (Effects like `Console`/`Log`/`Dep`
    * are no longer ambient — they are import-required from `eliot.effect` — so they need no filtering here.)
    */
  val systemModulesWithoutInt: Seq[ModuleName] =
    ModuleName.defaultSystemModules.filterNot(m => Set("Int", "Runtime").contains(m.name))
}
