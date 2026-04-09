package com.vanillasource.eliot.eliotc.eval2.util

import cats.data.Chain
import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Types
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression

class QuoterTest extends ProcessorTest() {

  "quoter" should "quote Lit to Direct value" in {
    val direct = Value.Direct(42, bigIntType)
    runQuote(Sem.Lit(direct)).asserting(_ shouldBe Some(direct))
  }

  it should "quote string Lit" in {
    val direct = Value.Direct("hello", stringType)
    runQuote(Sem.Lit(direct)).asserting(_ shouldBe Some(direct))
  }

  it should "quote TypeUniv to Value.Type" in {
    runQuote(Sem.TypeUniv).asserting(_ shouldBe Some(Value.Type))
  }

  it should "quote Struct to Value.Structure" in {
    val fqn    = ValueFQN(
      ModuleName(Seq("eliot", "lang"), "BigInteger"),
      QualifiedName("BigInteger", Qualifier.Type)
    )
    val sem    = Sem.Struct(fqn, Map.empty)
    runQuote(sem).asserting {
      case Some(Value.Structure(fields, Value.Type)) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(v: ValueFQN, _)) => v shouldBe fqn
          case other                               => fail(s"Expected $$typeName field, got: $other")
        }
      case other                                     => fail(s"Expected Some(Structure), got: $other")
    }
  }

  it should "quote Struct with fields" in {
    val fqn = Types.functionDataTypeFQN
    val sem = Sem.Struct(
      fqn,
      Map(
        "A" -> Sem.Lit(Value.Direct(0, bigIntType)),
        "B" -> Sem.TypeUniv
      )
    )
    runQuote(sem).asserting {
      case Some(Value.Structure(fields, Value.Type)) =>
        fields("A") shouldBe Value.Direct(0, bigIntType)
        fields("B") shouldBe Value.Type
      case other                                     => fail(s"Expected Some(Structure), got: $other")
    }
  }

  it should "return None for Lam" in {
    val lam = Sem.Lam(
      "x",
      Sem.TypeUniv,
      Closure(Env.empty, OperatorResolvedExpression.ParameterReference(sourced("x")))
    )
    runQuote(lam).asserting(_ shouldBe None)
  }

  it should "return None for Neut with Param head" in {
    runQuote(Sem.Neut(Head.Param("x"), Seq.empty)).asserting(_ shouldBe None)
  }

  it should "return None for Neut with Ref head" in {
    val vfqn = ValueFQN(testModuleName, QualifiedName("unknown", Qualifier.Default))
    runQuote(Sem.Neut(Head.Ref(vfqn), Seq.empty)).asserting(_ shouldBe None)
  }

  it should "return None for Neut with Meta head" in {
    runQuote(Sem.Neut(Head.Meta(MetaId(0)), Seq.empty)).asserting(_ shouldBe None)
  }

  it should "return None for Struct with unquotable field" in {
    val fqn = Types.functionDataTypeFQN
    val sem = Sem.Struct(fqn, Map("A" -> Sem.Neut(Head.Param("x"), Seq.empty)))
    runQuote(sem).asserting(_ shouldBe None)
  }

  private def runQuote(sem: Sem): IO[Option[Value]] =
    for {
      generator <- createGenerator(Seq.empty)
      result    <- Quoter.quote(sem).run(MetaState()).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, (_, v))) => v
      case Left(errors)       =>
        throw Exception(s"Quote failed: ${errors.toList.map(_.message).mkString(", ")}")
    }
}
