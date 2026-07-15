package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** The signature split, Step 5 (consumer-first): the `Signature` twin gets its own monomorphization. Requesting
  * `CompilerMonomorphicValue(v@Signature, args)` runs the *same* [[com.vanillasource.eliot.eliotc.monomorphize.check.TypeStackLoop]]
  * in `signatureOnly` mode — kind-check the signature body against its derived kind, elaborate + reduce on the compiler
  * track — with **no separate body to check** (walking the signature is the whole job) and a **W3 decline** (a
  * calculated return is solvable only by the body, which the signature twin lacks; the runtime twin owns that back-edge,
  * landed in Step 6).
  *
  * The consumer landing in the same arc is this **equivalence test**: for representative fixtures (non-generic, generic,
  * ability-constrained, W3-declined), the signature twin's ground signature equals what the in-place walk of the runtime
  * twin produces on the *same* (compiler) track — since only the body-driven return (calc / guard) can diverge the two,
  * and W3 is excluded by declining. All fixtures live in the **compiler** source pool.
  */
class SignatureTwinMonoTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {

  private def compilerScan(pkg: Seq[String], name: String, content: String): Seq[SourceContent | PathScan] = {
    val path = (pkg :+ s"$name.els").foldLeft(Path.of(""))(_ `resolve` _)
    val uri  = URI.create((pkg :+ s"$name.els").mkString("/"))
    Seq(
      PathScan(path, Seq(uri), Platform.Compiler),
      SourceContent(uri, Sourced(uri, PositionRange.zero, content))
    )
  }

  private val mContent =
    """import eliot.lang.Function
      |import eliot.lang.Bool
      |import eliot.lang.Eq
      |import eliot.lang.String
      |import eliot.effect.Throw
      |
      |type A
      |
      |def b: A
      |
      |def foo: A = b
      |
      |def id[X](x: X): X = x
      |
      |def eqParam[E ~ Eq](a: E): E = a
      |
      |def raiseGuard: {Throw[String]} Type = raise("empty")
      |
      |type Counter[auto N]
      |
      |def bump(c: Counter): Counter = c
      |""".stripMargin

  // The compile-time `Either` carrier plus the effect abilities it implements — the machinery a `{Throw[String]}` guard
  // signature reduces through (a faithful slice of `stdlib/eliot-compiler/eliot/lang/Either.els`, as in
  // [[CompilerAbilityResolutionTest]]). Needed only for the guarded fixture.
  private val eitherContent =
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

  private val facts: Seq[CompilerFact] =
    compilerScan(Seq("eliot", "compiler"), "Type", "type Type") ++
      compilerScan(Seq("eliot", "lang"), "Function", "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B") ++
      compilerScan(Seq("eliot", "lang"), "String", "type String") ++
      compilerScan(Seq("eliot", "lang"), "Bool", "type Bool\ndef true: Bool\ndef false: Bool") ++
      compilerScan(
        Seq("eliot", "lang"),
        "Eq",
        "import eliot.lang.Bool\nability Eq[A] { def equals(a: A, b: A): Bool }\nimplement Eq[Type] { def equals(a: Type, b: Type): Bool }"
      ) ++
      compilerScan(
        Seq("eliot", "effect"),
        "Effect",
        "import eliot.lang.Function\nability Effect[F[_]] {\n  def flatMap[A, B](f: Function[A, F[B]], fa: F[A]): F[B]\n  def pure[A](a: A): F[A]\n  def map[A, B](f: Function[A, B], fa: F[A]): F[B]\n}"
      ) ++
      compilerScan(
        Seq("eliot", "effect"),
        "Throw",
        "import eliot.lang.Function\nability Throw[E, F[_]] {\n  def raise[A](err: E): F[A]\n}"
      ) ++
      compilerScan(Seq("eliot", "lang"), "Either", eitherContent) ++
      compilerScan(Seq("test"), "M", mContent)

  private val mModule = ModuleName(Seq("test"), "M")

  private def fqn(name: String, qualifier: Qualifier = Qualifier.Default) =
    ValueFQN(mModule, QualifiedName(name, qualifier))

  /** Demand both twins' compiler monos for `fqn` at `args` from one generator (they coexist, as in a real compile) and
    * return the runtime twin's ground signature, the signature twin's ground signature, and any errors. */
  private def bothSignatures(
      value: ValueFQN,
      args: Seq[GroundValue]
  ): IO[(Option[GroundValue], Option[GroundValue], Seq[TestError])] =
    for {
      generator <- createGenerator(facts)
      runtime   <- generator.getFact(CompilerMonomorphicValue.Key(value, args))
      signature <- generator.getFact(CompilerMonomorphicValue.Key(value.copy(name = value.name.signatureTwin), args))
      errors    <- generator.currentErrors()
    } yield (runtime.map(_.signature), signature.map(_.signature), toTestErrors(errors))

  "the signature twin's compiler mono" should "match the runtime twin's for an abstract type constructor (`type A`)" in {
    bothSignatures(fqn("A", Qualifier.Type), Seq.empty)
      .asserting { case (rt, sig, errs) => (rt.isDefined, sig, errs) shouldBe (true, rt, Seq.empty) }
  }

  it should "match the runtime twin's for a non-generic abstract value (`b: A`)" in {
    bothSignatures(fqn("b"), Seq.empty)
      .asserting { case (rt, sig, errs) => (rt.isDefined, sig, errs) shouldBe (true, rt, Seq.empty) }
  }

  it should "match — even though the runtime twin also checks a body — for a bodied value (`foo: A = b`)" in {
    bothSignatures(fqn("foo"), Seq.empty)
      .asserting { case (rt, sig, errs) => (rt.isDefined, sig, errs) shouldBe (true, rt, Seq.empty) }
  }

  it should "match the runtime twin's for a generic value at concrete args (`id[X]` at `X = Type`)" in {
    bothSignatures(fqn("id"), Seq(GroundValue.Type))
      .asserting { case (rt, sig, errs) => (rt.isDefined, sig, errs) shouldBe (true, rt, Seq.empty) }
  }

  it should "match the runtime twin's for an ability-constrained value (`eqParam[E ~ Eq]` at `E = Type`)" in {
    bothSignatures(fqn("eqParam"), Seq(GroundValue.Type))
      .asserting { case (rt, sig, errs) => (rt.isDefined, sig, errs) shouldBe (true, rt, Seq.empty) }
  }

  it should "match the runtime twin's for a guarded (W2b) value — the pinned carrier `Either[String, Type]`" in {
    // The compiler track *produces* the undischarged carrier signature (pinCarriers fixes `{Throw[String]}`'s carrier to
    // the compile-time `Either[String]` before any body is seen), so the signature twin — which has no body — pins
    // identically and lands the same ground signature. This is the guard-producing path Step 7 will route through.
    bothSignatures(fqn("raiseGuard"), Seq.empty)
      .asserting { case (rt, sig, errs) => (rt.isDefined, sig, errs) shouldBe (true, rt, Seq.empty) }
  }

  "the signature twin's compiler mono for a calculated return (W3)" should "produce the under-applied return hole (§3.5)" in {
    // Signature-unification Phase B: the twin is an ordinary body mono, so it no longer *declines* a calculated return —
    // it produces the signature with the return left as an **under-applied constructor head** (`Counter` with zero
    // arguments), which the value mono's own body still solves via its return meta. The `Quoter` grounds the
    // body-less `VTopDef(Counter, None, …)` at any arity, so the hole rides the twin fact with no fact-shape change.
    bothSignatures(fqn("bump"), Seq(GroundValue.Type))
      .asserting { case (_, sig, errs) =>
        (sig.map(_.deepReturnType), errs) shouldBe
          (Some(GroundValue.Structure(fqn("Counter", Qualifier.Type), Seq.empty, GroundValue.Type)), Seq.empty)
      }
  }
}
