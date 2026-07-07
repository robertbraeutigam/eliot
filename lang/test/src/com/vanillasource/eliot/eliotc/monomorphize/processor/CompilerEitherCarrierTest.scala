package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, UnifiedModuleNames}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** CP4 leaf test: the compiler-platform `Either` carrier is well-formed Eliot and is
  * extracted into the **compiler** source pool — the concrete `data`/`foldEither` *and* the compile-time
  * `Effect[Either[String]]` / `Throw[String, Either[String]]` instances.
  *
  * The full-pipeline `Throw` integration tests already exercise `Either`/`Left`/`Right`/`foldEither` (through the jvm
  * runtime layer, with the compiler overlay applied transparently), but they never touch the bare `Either[String]`
  * ability instances — those are reached only by the (not-yet-landed) effectful-signatures discharge (W2). This test
  * pins that the `implement` blocks at least parse and lower into ability-implementation members under the `compiler`
  * marker, so a malformed carrier cannot ship unnoticed before its first consumer arrives.
  *
  * Name extraction runs entirely before resolution (`ModuleNames` reads a file's own declarations), so the carrier is
  * injected as a single `compiler`-marker [[PathScan]] with no base stubs: `import`s and inner references need not
  * resolve for the names to be collected.
  */
class CompilerEitherCarrierTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {
  private val eitherModule = ModuleName(Seq("eliot", "lang"), "Either")
  private val eitherPath   = Path.of("eliot", "lang", "Either.els")
  private val eitherUri    = URI.create("eliot/lang/Either.els")

  // A faithful copy of `stdlib/eliot-compiler/eliot/lang/Either.els`'s shape: the concrete carrier plus the two
  // compile-time ability instances. (The shipped file itself is exercised end-to-end by the LSP/jvm integration suites,
  // which point the compiler path at the real resource dir.)
  private val carrier =
    """import eliot.effect.Effect
      |import eliot.effect.Throw
      |
      |data Either[E, A] = Left(error: E) | Right(value: A)
      |
      |def foldEither[E, A, B](e: Either[E, A], ifLeft: Function[E, B], ifRight: Function[A, B]): B = e match {
      |   case Left(err) -> ifLeft(err)
      |   case Right(v) -> ifRight(v)
      |}
      |
      |implement Effect[Either[String]] {
      |   def pure[A](a: A): Either[String, A] = Right(a)
      |   def flatMap[A, B](f: Function[A, Either[String, B]], fa: Either[String, A]): Either[String, B] =
      |      foldEither(fa, err -> Left(err), a -> f(a))
      |   def map[A, B](f: Function[A, B], fa: Either[String, A]): Either[String, B] =
      |      foldEither(fa, err -> Left(err), a -> Right(f(a)))
      |}
      |
      |implement Throw[String, Either[String]] {
      |   def raise[A](err: String): Either[String, A] = Left(err)
      |}
      |""".stripMargin

  private val compilerPool = Seq(
    PathScan(eitherPath, Seq(eitherUri), Platform.Compiler),
    SourceContent(eitherUri, Sourced(eitherUri, PositionRange.zero, carrier))
  )

  private def compilerNames: IO[Set[QualifiedName]] =
    runGeneratorWithFacts(compilerPool, UnifiedModuleNames.Key(eitherModule, Platform.Compiler))
      .map(_._1.fold(Set.empty[QualifiedName])(_.names.keySet))

  // The ability names of every ability-implementation member declared in the module.
  private def implementedAbilities(names: Set[QualifiedName]): Set[String] =
    names.collect { case QualifiedName(_, Qualifier.AbilityImplementation(name, _)) => name.value }

  "the compiler-platform Either carrier" should "extract the concrete data names under the compiler marker" in {
    compilerNames.asserting(
      _ should contain allOf (
        QualifiedName("Either", Qualifier.Type),
        QualifiedName("Left", Qualifier.Default),
        QualifiedName("Right", Qualifier.Default),
        QualifiedName("foldEither", Qualifier.Default)
      )
    )
  }

  it should "carry the compile-time Effect and Throw instances" in {
    compilerNames.map(implementedAbilities).asserting(_ should contain allOf ("Effect", "Throw"))
  }
}
