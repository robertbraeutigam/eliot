package com.vanillasource.eliot.eliotc.resolve

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName => ModuleName2, QualifiedName, Qualifier, Role, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.ExpressionMatchers.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The signature split, Step 2: the `Signature` twin resolves as a value in its own right, through the *same*
  * [[com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver]] as any runtime value — no role-specific path. Its
  * body is the (resolved) signature expression, and the generic binders baked into that expression are in scope while it
  * resolves (the cross-twin scope point: the same binders the runtime twin's body resolves against). The runtime twin is
  * untouched (its `ResolvedValue` is byte-identical), which the existing `ValueResolverTest` already pins.
  */
class SignatureTwinResolveTest extends ProcessorTest(LangProcessors(systemModules = Seq(ModuleName2.systemFunctionModuleName))*) {
  private val testModuleName2 = ModuleName2(Seq.empty, "Test")

  "the signature twin" should "resolve as its own value with the Signature role" in {
    resolveSignatureTwin("data T\ndef a: T", "a").asserting(_.map(_.vfqn.name.role) shouldBe Some(Role.Signature))
  }

  it should "carry the resolved signature expression as its body" in {
    // `def a: T` — the signature twin's body is the type `T`, resolving to a reference to the `T` type constructor.
    resolveSignatureTwinBody("data T\ndef a: T", "a").flatMap {
      case Some(ValueReference(Sourced(_, _, vfqn), _)) =>
        IO.delay(vfqn shouldBe ValueFQN(testModuleName2, QualifiedName("T", Qualifier.Type)))
      case x                                            => IO.delay(fail(s"was not a type reference, instead: $x"))
    }
  }

  it should "produce no errors for an abstract value's signature twin" in {
    resolveErrors("data T\ndef a: T", "a").asserting(_ shouldBe Seq.empty)
  }

  it should "keep the generic binder in scope while resolving the signature body (cross-twin scope)" in {
    // `def id[X](x: X): X` — the signature twin's body references `X`, which must resolve as an in-scope binder, not
    // abort with "Name not defined." (the parameter references are how we know the binder scope carried across).
    resolveErrors("def id[X](x: X): X = x", "id").asserting(_ shouldBe Seq.empty)
  }

  private def signatureTwinFqn(name: String): ValueFQN =
    ValueFQN(testModuleName2, QualifiedName(name, Qualifier.Default, Role.Signature))

  private def resolveSignatureTwin(source: String, name: String): IO[Option[ResolvedValue]] =
    runGenerator(source, ResolvedValue.Key(signatureTwinFqn(name)), systemImports)
      .map { case (_, facts) =>
        facts.values.collectFirst { case rv: ResolvedValue if rv.vfqn == signatureTwinFqn(name) => rv }
      }

  private def resolveSignatureTwinBody(source: String, name: String): IO[Option[Expression]] =
    resolveSignatureTwin(source, name).map(_.flatMap(_.runtime.map(_.value)))

  private def resolveErrors(source: String, name: String): IO[Seq[String]] =
    runGenerator(source, ResolvedValue.Key(signatureTwinFqn(name)), systemImports)
      .map { case (errors, _) => toTestErrors(errors).map(_.message) }
}
