package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.VTopDef
import com.vanillasource.eliot.eliotc.monomorphize.fact.{ContributedBinding, NativeBinding, TransparentBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** CP3 leaf test: the `compiler` native label makes the **compiler** source pool a binding supplier. A name concrete
  * only in the compiler platform reduces during checking off its compiler-platform body (the merger prefers the native
  * over the runtime user body); the *same* name's runtime body is what `TransparentBinding`/codegen uses. The two pools
  * are injected as one [[PathScan]] per marker — `foo` is concretely defined in **both**, with a distinct body each, so
  * the two markers can be told apart by which reference the binding reduces to.
  */
class CompilerNativesProcessorTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {
  private val moduleName  = ModuleName(Seq.empty, "M")
  private val modulePath  = Path.of("M.els")
  private val compilerUri = URI.create("compilerM.els") // compiler platform: `foo` reduces to `b`
  private val runtimeUri  = URI.create("runtimeM.els")  // jvm/runtime layer: `foo` reduces to `c`

  private def fqn(name: String) = ValueFQN(moduleName, QualifiedName(name, Qualifier.Default))

  private def source(uri: URI, content: String): SourceContent =
    SourceContent(uri, Sourced(uri, PositionRange.zero, content))

  /** Two distinct pools for module `M`. Both declare `b` and `c` (so the body references resolve identically in both
    * pools — as the compiler-platform `Either`'s dependencies do via jvm's runtime `Either`), but `foo`'s body differs
    * per pool: the compiler pool reduces it to `b`, the runtime pool to `c`. `d` exists only in the runtime pool, to
    * probe the contributor's totality for a name absent from the compiler pool.
    */
  private val twoPoolFacts = Seq(
    PathScan(modulePath, Seq(compilerUri), Platform.Compiler),
    PathScan(modulePath, Seq(runtimeUri), Platform.Runtime),
    source(compilerUri, "type A\ndef b: A\ndef c: A\ndef foo: A = b"),
    source(runtimeUri, "type A\ndef b: A\ndef c: A\ndef d: A\ndef foo: A = c")
  )

  /** The FQN the (forced) binding for `foo` reduces to — `Some("b")` for the compiler body, `Some("c")` for the
    * runtime body. `foo`'s binding is a `VTopDef` carrying a lazy thunk; forcing it evaluates the body, an unbound
    * reference that stays an FQN-preserving stuck `VTopDef`.
    */
  private def reducesTo(sem: SemValue): Option[String] = sem match {
    case VTopDef(_, Some(cached), _) =>
      cached.value match {
        case VTopDef(target, _, _) => Some(target.name.name)
        case _                     => None
      }
    case _                           => None
  }

  private def fact[K <: com.vanillasource.eliot.eliotc.processor.CompilerFact](
      trigger: com.vanillasource.eliot.eliotc.processor.CompilerFactKey[K]
  ): IO[(Option[K], Seq[TestError])] =
    runGeneratorWithFacts(twoPoolFacts, trigger).map { case (result, errors) => (result, toTestErrors(errors)) }

  "the compiler native label" should "contribute the compiler-platform body for a name defined there" in {
    fact(ContributedBinding.Key(fqn("foo"), ContributedBinding.compilerLabel))
      .asserting(_._1.flatMap(_.contributed).isDefined shouldBe true)
  }

  it should "reduce a name during checking off its compiler-platform body (native wins over the runtime user body)" in {
    fact(NativeBinding.Key(fqn("foo"))).asserting(_._1.map(b => reducesTo(b.semValue)) shouldBe Some(Some("b")))
  }

  it should "use the runtime body for codegen (TransparentBinding)" in {
    fact(TransparentBinding.Key(fqn("foo"))).asserting(_._1.map(b => reducesTo(b.semValue)) shouldBe Some(Some("c")))
  }

  it should "contribute None for a name absent from the compiler pool" in {
    fact(ContributedBinding.Key(fqn("d"), ContributedBinding.compilerLabel))
      .asserting(_._1.flatMap(_.contributed) shouldBe None)
  }

  it should "raise no spurious error querying the compiler label for a runtime-only name" in {
    fact(ContributedBinding.Key(fqn("d"), ContributedBinding.compilerLabel)).asserting(_._2 shouldBe Seq.empty)
  }
}
