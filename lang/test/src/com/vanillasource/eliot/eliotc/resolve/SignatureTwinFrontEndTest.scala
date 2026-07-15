package com.vanillasource.eliot.eliotc.resolve

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredValue
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName => ModuleName2, QualifiedName, Qualifier, Role, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import org.scalatest.compatible.Assertion

/** The signature split, Steps 3–4: confirmation that the `Signature` twin flows through the rest of the front-end —
  * `matchdesugar`, `operator`, and `saturate` — as a value in its own right, needing no production change. These phases
  * are all keyed by `(vfqn, platform)` and role-agnostic, so the signature twin (a well-formed value whose body is its
  * signature expression) is desugared, operator-resolved, and saturated exactly like any runtime value. Effect/recursion
  * checks stay runtime-only for free (nothing references a signature twin, so they are never demanded for one) — their
  * extension to signature bodies is Step 8.
  */
class SignatureTwinFrontEndTest extends ProcessorTest(LangProcessors(systemModules = Seq(ModuleName2.systemFunctionModuleName))*) {
  private val testModuleName2 = ModuleName2(Seq.empty, "Test")

  private def signatureTwinFqn(name: String, qualifier: Qualifier = Qualifier.Default): ValueFQN =
    ValueFQN(testModuleName2, QualifiedName(name, qualifier, Role.Signature))

  "matchdesugar" should "produce the signature twin as its own value, body preserved, error-free" in {
    frontEnd("def id[X](x: X): X = x", MatchDesugaredValue.Key(signatureTwinFqn("id"))) { md =>
      md.vfqn.name.role shouldBe Role.Signature
      md.runtime.isDefined shouldBe true
    }
  }

  "operator" should "produce the signature twin as its own value, body preserved, error-free" in {
    frontEnd("def id[X](x: X): X = x", OperatorResolvedValue.Key(signatureTwinFqn("id"))) { op =>
      op.vfqn.name.role shouldBe Role.Signature
      op.runtime.isDefined shouldBe true
    }
  }

  "saturate" should "produce the signature twin as its own value, error-free" in {
    frontEnd("def id[X](x: X): X = x", SaturatedValue.Key(signatureTwinFqn("id"))) { sat =>
      sat.value.vfqn.name.role shouldBe Role.Signature
      sat.value.runtime.isDefined shouldBe true
    }
  }

  it should "carry the signature twin's leading auto-marked binder count (inferableArity, per §5)" in {
    frontEnd("type Pair[auto A, B]", SaturatedValue.Key(signatureTwinFqn("Pair", Qualifier.Type))) { sat =>
      sat.value.inferableArity shouldBe 1
    }
  }

  /** Demand `key`, assert no compiler errors, find the produced fact for the requested (role-bearing) key, and run the
    * caller's structural assertions on it.
    */
  private def frontEnd[K <: CompilerFact](source: String, key: CompilerFactKey[K])(check: K => Assertion): IO[Assertion] =
    runGenerator(source, key, systemImports).map { case (errors, facts) =>
      toTestErrors(errors).map(_.message) shouldBe Seq.empty
      facts.get(key) match {
        case Some(fact) => check(fact.asInstanceOf[K])
        case None       => fail(s"no fact produced for $key")
      }
    }
}
