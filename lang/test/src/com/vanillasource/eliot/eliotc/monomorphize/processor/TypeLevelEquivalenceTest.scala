package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** Step A of the type-levels-as-values plan (docs/type-levels-as-values.md): a value's level-`n` *type expression* is a
  * named value run on the compiler track. This suite pins the **equivalence** the plan asks for — the level-1 value
  * ([[CompilerMonomorphicValue]] at `typeLevel = 1`) reduces its body to exactly the type today's `walkTypeStack`
  * produces for that value's signature at the same type arguments (its level-0 `.signature`).
  *
  * The level-1 value's `.reduced` is the *type expression evaluated as a body* — a small `MonomorphicExpression` denoting
  * a type. [[denote]] reads that pure type expression back to a [[GroundValue]] so it can be compared against the host's
  * level-0 signature directly.
  *
  * Scope (per the plan's "verified equivalent to today" + "divergence is a finding"): the fixtures here are pure
  * signatures and generic values whose level body does not reference a *type-kinded* generic parameter as a type. The one
  * known Step-A divergence — a `Function[X, X]`-style body that references a `X: Type` parameter — is pinned by
  * [[typeKindedParameterInBody]] as a finding, not silently accepted; it is resolved when Step B/C rework how a level
  * body is checked (a level body must resolve a type parameter through ρ, as `evalExpr` does in the signature walk, not
  * through Γ, where the "types are values" binding gives the parameter its instantiated *value*'s type slot). The
  * effectful `orError`-combinator forms named in the plan carry a *`Bool`-kinded* parameter (`COND: Bool`), which does
  * *not* hit that divergence; their end-to-end equivalence is exercised when Step B lands the effectful-return read.
  */
class TypeLevelEquivalenceTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {
  private def source(uri: URI, content: String): SourceContent =
    SourceContent(uri, Sourced(uri, PositionRange.zero, content))

  /** A module declared in both the compiler and runtime pools (the compiler track probes runtime membership). */
  private def bothPools(module: ModuleName, content: String): Seq[CompilerFact] = {
    val path = Path.of(module.packages.mkString("/"), module.name + ".els")
    val cUri = URI.create("c/" + module.packages.mkString("/") + "/" + module.name + ".els")
    val rUri = URI.create("r/" + module.packages.mkString("/") + "/" + module.name + ".els")
    Seq(
      PathScan(path, Seq(cUri), Platform.Compiler),
      PathScan(path, Seq(rUri), Platform.Runtime),
      source(cUri, content),
      source(rUri, content)
    )
  }

  private val typeModule = ModuleName(Seq("eliot", "compiler"), "Type")
  private val funcModule = ModuleName(Seq("eliot", "lang"), "Function")
  private val mModule    = ModuleName(Seq.empty, "M")

  private val facts: Seq[CompilerFact] =
    bothPools(typeModule, "type Type") ++
      bothPools(funcModule, "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B") ++
      bothPools(
        mModule,
        """import eliot.lang.Function
          |type A
          |def b: A
          |def fnAA: Function[A, A]
          |def zero: A = b
          |def arrow: Function[A, A] = fnAA
          |def genConst[X]: A = b
          |def genArrow[X]: Function[X, X] = fnXX
          |def fnXX[X]: Function[X, X]
          |""".stripMargin
      )

  private def fqn(name: String)  = ValueFQN(mModule, QualifiedName(name, Qualifier.Default))
  private def tfqn(name: String) = ValueFQN(mModule, QualifiedName(name, Qualifier.Type))
  private val typeA              = GroundValue.Structure(tfqn("A"), Seq.empty, GroundValue.Type)

  private def mono(name: String, args: Seq[GroundValue], level: Int) =
    runGeneratorWithFacts(facts, CompilerMonomorphicValue.Key(fqn(name), args, level)).map {
      case (result, errors) => (result, toTestErrors(errors))
    }

  /** Read a *pure* type-denoting reduced body back to the [[GroundValue]] it denotes: a value reference to a type
    * constructor becomes a [[GroundValue.Structure]], and a `Function[dom, cod]` application chain folds its arguments
    * onto that structure. Enough for the pure-signature fixtures here; effectful bodies are Step B's concern.
    */
  private def denote(expr: MonomorphicExpression.Expression): GroundValue = expr match {
    case MonomorphicExpression.MonomorphicValueReference(name, typeArgs) => GroundValue.Structure(name.value, typeArgs, GroundValue.Type)
    case MonomorphicExpression.FunctionApplication(target, argument)     =>
      denote(target.value.expression) match {
        case GroundValue.Structure(name, args, vt) => GroundValue.Structure(name, args :+ denote(argument.value.expression), vt)
        case other                                 => other
      }
    case _                                                               => GroundValue.Type
  }

  /** Assert the level-1 value reduces its body to the same type today's walk produces as the level-0 signature. */
  private def assertEquivalent(name: String, args: Seq[GroundValue]) =
    for {
      l0 <- mono(name, args, 0)
      l1 <- mono(name, args, 1)
    } yield {
      l1._2 shouldBe Seq.empty
      l0._1.map(_.signature) shouldBe l1._1.flatMap(_.reduced).map(r => denote(r.value))
    }

  "a non-generic value's level-1 body" should "reduce to its level-0 signature (a plain type)" in {
    assertEquivalent("zero", Seq.empty)
  }

  "a non-generic value's level-1 body" should "reduce to its level-0 signature (a Function arrow)" in {
    assertEquivalent("arrow", Seq.empty)
  }

  "a generic value whose signature ignores its parameter" should "reduce its level-1 body to its instantiated level-0 signature" in {
    assertEquivalent("genConst", Seq(typeA))
  }

  // Formerly a pinned Step-A divergence: a level body referencing a `X: Type` parameter as a type routes through the
  // value-inference path. `applyTypeArgs` now binds the parameter's Γ slot to its *kind* (`Type`), not its instantiated
  // value (`A`), so `Function[X, X]` kind-checks against `Type` and reduces exactly like the type-position walk — one
  // path, per docs/type-levels-as-values.md §2.
  "a level body referencing a type-kinded parameter" should "reduce its level-1 body to its instantiated level-0 signature" in {
    assertEquivalent("genArrow", Seq(typeA))
  }
}
