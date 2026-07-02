package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** Compiler-as-platform Increments B, C **and** D: the compiler monomorphize track resolves ability instances **in the
  * compiler pool** (B), **reduces** the resolved call to its normal form (C — the compiler backend), and **pins the
  * effect carrier** of a `{Throw[String]}` sugar signature to the compile-time `Either[String]` (D).
  *
  * B/C — a `pure`/`raise` call in a compiler-platform value with an *explicit* `Either[String, _]` return dispatches to
  * the compile-time `Effect[Either[String]]` / `Throw[String, Either[String]]` implementations (the reduced form carries
  * the concrete `Either::pure` / `Either::raise`, not the abstract `Effect::pure` / `Throw::raise` ability method), and
  * the compiler backend then folds the concrete-impl *body* in by ordinary NbE evaluation (`Either::raise`'s
  * `err -> Left(err)`), so `raise("boom")` reduces all the way to `Left("boom")` and `pure("hello")` to `Right("hello")`.
  *
  * D — the same works when the carrier is written with the `{Throw[String]}` *effect sugar* on a `Type` return instead
  * of an explicit `Either`. `EffectSugarDesugarer` turns `{Throw[String]} Type` into an inferable higher-kinded carrier
  * `[F[_] ~ Throw[String]]` with return `F[Type]`; the compiler platform *is* the runner, so on the compiler track that
  * carrier has no runtime value to infer it from and is pinned to the compile-time `Throw` carrier `Either[String]`
  * (`TypeStackLoop.pinCompilerCarriers`, the `main`-pins-`IO` analogue). Then `raise`/`pure` dispatch to the compile-time
  * instances exactly as in the explicit form. The compiler track *produces* the reduced guarded value (the discharge
  * `Right(t) ⤳ t` / `Left(msg) ⤳ error` is the runtime consumer's job, so it is skipped here), publishing the
  * undischarged carrier signature `Either[String, Type]`.
  *
  * The whole scenario lives in the **compiler** source pool (no runtime layer), so this pins that resolution targets the
  * compiler platform, not the default runtime one. The guarding fix is `TypeStackLoop.abilityArity` reading the marker
  * signature from the track's platform: before it, the arity query hit the (empty) runtime pool, the method reference's
  * type arguments were never sliced to the ability prefix, and resolution silently never fired.
  */
class CompilerAbilityResolutionTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {

  private def compilerScan(pkg: Seq[String], name: String, content: String): Seq[SourceContent | PathScan] = {
    val path = (pkg :+ s"$name.els").foldLeft(Path.of(""))(_ `resolve` _)
    val uri  = URI.create((pkg :+ s"$name.els").mkString("/"))
    Seq(
      PathScan(path, Seq(uri), Platform.Compiler),
      SourceContent(uri, Sourced(uri, PositionRange.zero, content))
    )
  }

  // The compile-time carrier (a faithful copy of `compiler/eliot/eliot/lang/Either.els`) plus the abstract effect
  // abilities it implements and the leaf stubs (`Type`, `Function`, `String`) they reference. Everything is compiler-pool.
  // `Type` is stubbed because the Increment-D sugar values (`{Throw[String]} Type`) name it in the carrier's kind.
  private val facts: Seq[com.vanillasource.eliot.eliotc.processor.CompilerFact] =
    compilerScan(Seq("eliot", "compiler"), "Type", "type Type") ++
      compilerScan(Seq("eliot", "lang"), "Function", "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B") ++
      compilerScan(Seq("eliot", "lang"), "String", "type String") ++
      compilerScan(
        Seq("eliot", "effect"),
        "Effect",
        "import eliot.lang.Function\nability Effect[F[_]] {\n  def flatMap[A, B](fa: F[A], f: Function[A, F[B]]): F[B]\n  def pure[A](a: A): F[A]\n  def map[A, B](fa: F[A], f: Function[A, B]): F[B]\n}"
      ) ++
      compilerScan(
        Seq("eliot", "effect"),
        "Throw",
        "import eliot.lang.Function\nability Throw[E, F[_]] {\n  def raise[A](err: E): F[A]\n}"
      ) ++
      compilerScan(
        Seq("eliot", "lang"),
        "Either",
        """import eliot.lang.Function
          |import eliot.lang.String
          |import eliot.effect.Effect
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
          |   def flatMap[A, B](fa: Either[String, A], f: Function[A, Either[String, B]]): Either[String, B] =
          |      foldEither(fa, err -> Left(err), a -> f(a))
          |   def map[A, B](fa: Either[String, A], f: Function[A, B]): Either[String, B] =
          |      foldEither(fa, err -> Left(err), a -> Right(f(a)))
          |}
          |
          |implement Throw[String, Either[String]] {
          |   def raise[A](err: String): Either[String, A] = Left(err)
          |}
          |""".stripMargin
      ) ++
      compilerScan(
        Seq("test"),
        "M",
        """import eliot.lang.Function
          |import eliot.lang.String
          |import eliot.lang.Either
          |import eliot.effect.Effect
          |import eliot.effect.Throw
          |
          |def raiseConst: Either[String, String] = raise("boom")
          |def pureConst: Either[String, String] = pure("hello")
          |
          |def raiseGuard: {Throw[String]} Type = raise("empty")
          |def someFn(A: Type): {Throw[String]} Type = pure(A)
          |
          |def gerror[A](msg: String): Either[String, A] = raise(msg)
          |""".stripMargin
      )

  private def fqn(name: String) = ValueFQN(ModuleName(Seq("test"), "M"), QualifiedName(name, Qualifier.Default))

  private def reducedOf(name: String): IO[Option[MonomorphicExpression.Expression]] =
    runGeneratorWithFacts(facts, CompilerMonomorphicValue.Key(fqn(name), Seq.empty)).map(_._1.flatMap(_.reduced.map(_.value)))

  private def errorsOf(name: String): IO[Seq[TestError]] =
    runGeneratorWithFacts(facts, CompilerMonomorphicValue.Key(fqn(name), Seq.empty)).map { case (_, e) => toTestErrors(e) }

  /** The value references reachable in a reduced body, drilling through applications and lambdas, as `(module, name)`. */
  private def valueRefs(expr: MonomorphicExpression.Expression): Seq[(String, String)] = expr match {
    case MonomorphicExpression.MonomorphicValueReference(vfqn, _)    => Seq((vfqn.value.moduleName.name, vfqn.value.name.name))
    case MonomorphicExpression.FunctionApplication(target, argument) =>
      valueRefs(target.value.expression) ++ valueRefs(argument.value.expression)
    case MonomorphicExpression.FunctionLiteral(_, _, body)           => valueRefs(body.value.expression)
    case _                                                           => Seq.empty
  }

  private def refsOf(name: String): IO[Seq[(String, String)]] = reducedOf(name).map(_.toSeq.flatMap(valueRefs))

  /** The string literals surviving in a reduced body — the guard/error payload carried through the reduction. */
  private def stringLiteralsOf(expr: MonomorphicExpression.Expression): Seq[String] = expr match {
    case MonomorphicExpression.StringLiteral(v)                     => Seq(v.value)
    case MonomorphicExpression.FunctionApplication(target, argument) =>
      stringLiteralsOf(target.value.expression) ++ stringLiteralsOf(argument.value.expression)
    case MonomorphicExpression.FunctionLiteral(_, _, body)          => stringLiteralsOf(body.value.expression)
    case _                                                          => Seq.empty
  }

  private def litsOf(name: String): IO[Seq[String]] = reducedOf(name).map(_.toSeq.flatMap(stringLiteralsOf))

  /** The FQN heading a value's monomorphic signature return type — `Either` for an undischarged `{Throw[String]}`
    * carrier signature.
    */
  private def returnHeadOf(name: String): IO[Option[(String, String)]] =
    runGeneratorWithFacts(facts, CompilerMonomorphicValue.Key(fqn(name), Seq.empty)).map { case (r, _) =>
      r.flatMap(_.signature.deepReturnType.typeFQN.map(t => (t.moduleName.name, t.name.name)))
    }

  "the compiler track (Increments B+C)" should "check a value using `raise` without error" in {
    errorsOf("raiseConst").asserting(_ shouldBe Seq.empty)
  }

  it should "reduce `raise(m)` to the concrete `Left(m)` — resolve the `Throw` impl and fold its body" in {
    refsOf("raiseConst").asserting(_ shouldBe Seq(("Either", "Left")))
  }

  it should "carry the raised message into the reduced `Left`" in {
    litsOf("raiseConst").asserting(_ shouldBe Seq("boom"))
  }

  it should "reduce `pure(x)` to the concrete `Right(x)` — resolve the `Effect` impl and fold its body" in {
    refsOf("pureConst").asserting(_ shouldBe Seq(("Either", "Right")))
  }

  it should "carry the pured value into the reduced `Right`" in {
    litsOf("pureConst").asserting(_ shouldBe Seq("hello"))
  }

  "the compiler track (Increment D — carrier pinning)" should "check a `{Throw[String]} Type` sugar guard without error" in {
    errorsOf("raiseGuard").asserting(_ shouldBe Seq.empty)
  }

  it should "reduce `raise(m)` under the pinned sugar carrier to `Left(m)` — no explicit `Either` written" in {
    refsOf("raiseGuard").asserting(_ shouldBe Seq(("Either", "Left")))
  }

  it should "carry the raised message through the pinned-carrier reduction" in {
    litsOf("raiseGuard").asserting(_ shouldBe Seq("empty"))
  }

  it should "publish the undischarged compile-time carrier `Either[String, _]` as the guard's return type" in {
    returnHeadOf("raiseGuard").asserting(_ shouldBe Some(("Either", "Either")))
  }

  it should "dispatch `pure` in a `{Throw[String]} Type` guard to the concrete `Either::pure`, not the abstract `Effect::pure`" in {
    // The doc's canonical first client `someFn(A: Type): {Throw[String]} Type = pure(A)`. The pin fixes the carrier to
    // `Either[String]`, so `pure` resolves to the compile-time `Effect[Either[String]]` impl (module `Either`); the fold
    // to `Right(A)` is deferred to the consumer because the payload `A` is a neutral parameter (a compiler-backend trait).
    refsOf("someFn").asserting(_ shouldBe Seq(("Either", "pure")))
  }

  it should "type the guard producer `someFn` without error" in {
    errorsOf("someFn").asserting(_ shouldBe Seq.empty)
  }

  it should "publish `someFn`'s undischarged carrier return `Either[String, Type]`" in {
    returnHeadOf("someFn").asserting(_ shouldBe Some(("Either", "Either")))
  }

  // Increment E — the guard combinators on the effect path. The shipped `eliot.lang.Guard` defines the bare rejection as
  // `error(msg) = raise(msg)` over the explicit `Either` carrier (and `orError(o, msg) = foldOption(o, error(msg), pure)`
  // building on it). The compiler backend reduces the generic `error[A]` combinator here, `raise` ⤳ the concrete
  // `Either::raise`; the full `orError` (with `foldOption` + the runtime-track consumption of the reduced native) is
  // exercised end-to-end in `GuardSignatureIntegrationTest`.

  "the compiler track (Increment E — guard combinators)" should "reduce the generic `error` guard's `raise` to the concrete `Either::raise`" in {
    refsOf("gerror").asserting(_ shouldBe Seq(("Either", "raise")))
  }
}
