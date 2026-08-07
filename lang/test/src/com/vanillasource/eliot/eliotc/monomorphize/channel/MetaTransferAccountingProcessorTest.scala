package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.plugin.LangProcessors

/** R2 enforcement (docs/total-meta-transfers.md §P2): a **body-less** value (a native leaf) that produces a
  * meta-carrying type must **state** a `^Meta` transfer; a bodied value derives and states nothing, and a leaf whose
  * return is a type parameter (a forwarded, not originated, meta) is exempt.
  *
  * A meta-carrying type here is one declaring `$Meta` slots (`type Gauge {level: Interval[BigInteger]}`, mirroring
  * production's `type Int {range: Interval[BigInteger]}`); the check is demanded directly at each value's
  * [[MetaTransferAccounting]] key, since the processor is not yet armed as a codegen precondition.
  */
class MetaTransferAccountingProcessorTest extends ProcessorTest(LangProcessors()*) {

  private val prelude =
    """type Gauge {level: Interval[BigInteger]}
      |""".stripMargin

  // Not named `key`: that collides with ScalaTest's `contain key(...)` DSL word, which silently wins the single-argument
  // call and never equals the produced fact key.
  private def accountingKey(name: String, typeArgs: Seq[GroundValue] = Seq.empty): MetaTransferAccounting.Key =
    MetaTransferAccounting.Key(ValueFQN(testModuleName, QualifiedName(name, Qualifier.Default)), typeArgs)

  private val gauge: GroundValue =
    GroundValue.Structure(ValueFQN(testModuleName, QualifiedName("Gauge", Qualifier.Type)), Seq.empty, GroundValue.Type)

  private val missingTransfer = "carries meta-information, but states no meta transfer"

  "a body-less leaf producing a meta-carrying type" should "be rejected when it states no transfer" in {
    runGenerator(prelude + "def readGauge: Gauge", accountingKey("readGauge"), ambientStubsWith()).map {
      case (errors, _) => errors.map(_.message).mkString("\n") should include(missingTransfer)
    }
  }

  "a body-less leaf producing a meta-carrying type" should "be accepted when it states a transfer" in {
    runGenerator(prelude + "def readGauge: Gauge {level(readGauge)}", accountingKey("readGauge"), ambientStubsWith())
      .map { case (errors, facts) =>
        errors.map(_.message).mkString("\n") should not include missingTransfer
        facts.keySet should contain(accountingKey("readGauge"))
      }
  }

  "a bodied value producing a meta-carrying type" should "be accepted (it derives, states nothing)" in {
    runGenerator(prelude + "def echo(g: Gauge): Gauge = g", accountingKey("echo"), ambientStubsWith()).map {
      case (errors, facts) =>
        errors.map(_.message).mkString("\n") should not include missingTransfer
        facts.keySet should contain(accountingKey("echo"))
    }
  }

  "a body-less leaf producing a non-meta type" should "be accepted" in {
    runGenerator(prelude + "def readName: String", accountingKey("readName"), ambientStubsWith()).map {
      case (errors, facts) =>
        errors.map(_.message).mkString("\n") should not include missingTransfer
        facts.keySet should contain(accountingKey("readName"))
    }
  }

  "a body-less leaf whose return is a type parameter" should "be exempt even at a meta-carrying instantiation" in {
    runGenerator(prelude + "def forward[A](a: A): A", accountingKey("forward", Seq(gauge)), ambientStubsWith()).map {
      case (errors, facts) =>
        errors.map(_.message).mkString("\n") should not include missingTransfer
        facts.keySet should contain(accountingKey("forward", Seq(gauge)))
    }
  }
}
