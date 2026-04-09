package com.vanillasource.eliot.eliotc.eval2.util

import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, fullyQualifiedNameType, stringType}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.Sem
import com.vanillasource.eliot.eliotc.eval2.fact.Sem.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuoterTest extends AnyFlatSpec with Matchers {
  private val testFqn = ValueFQN(ModuleName(Seq.empty, "Test"), QualifiedName("TestType", Qualifier.Type))

  "quoter" should "quote integer literal" in {
    Quoter.quote(Sem.Lit(Value.Direct(42, bigIntType))) shouldBe Some(Value.Direct(42, bigIntType))
  }

  it should "quote string literal" in {
    Quoter.quote(Sem.Lit(Value.Direct("hello", stringType))) shouldBe Some(Value.Direct("hello", stringType))
  }

  it should "quote TypeUniv to Value.Type" in {
    Quoter.quote(Sem.TypeUniv) shouldBe Some(Value.Type)
  }

  it should "quote empty Struct to Value.Structure with typeName" in {
    val result = Quoter.quote(Sem.Struct(testFqn, Map.empty))
    result shouldBe Some(
      Value.Structure(
        Map("$typeName" -> Value.Direct(testFqn, fullyQualifiedNameType)),
        Value.Type
      )
    )
  }

  it should "quote Struct with fields" in {
    val sem = Sem.Struct(
      testFqn,
      Map("A" -> Sem.Lit(Value.Direct(42, bigIntType)))
    )
    val result = Quoter.quote(sem)
    result shouldBe Some(
      Value.Structure(
        Map(
          "$typeName" -> Value.Direct(testFqn, fullyQualifiedNameType),
          "A"         -> Value.Direct(42, bigIntType)
        ),
        Value.Type
      )
    )
  }

  it should "quote nested Struct" in {
    val inner = Sem.Struct(testFqn, Map.empty)
    val outer = Sem.Struct(testFqn, Map("inner" -> inner))
    val result = Quoter.quote(outer)
    result.isDefined shouldBe true
  }

  it should "return None for free parameter" in {
    Quoter.quote(Sem.Neut(Head.Param("x"), Seq.empty)) shouldBe None
  }

  it should "return None for parameter with spine" in {
    Quoter.quote(Sem.Neut(Head.Param("f"), Seq(Sem.Lit(Value.Direct(1, bigIntType))))) shouldBe None
  }

  it should "return None for Lam" in {
    val lam = Sem.Lam(
      "x",
      Sem.TypeUniv,
      Closure(Env.empty, null) // body not used in quote
    )
    Quoter.quote(lam) shouldBe None
  }

  it should "quote bare Ref to simple type structure" in {
    val result = Quoter.quote(Sem.Neut(Head.Ref(testFqn), Seq.empty))
    result shouldBe Some(
      Value.Structure(
        Map("$typeName" -> Value.Direct(testFqn, fullyQualifiedNameType)),
        Value.Type
      )
    )
  }

  it should "return None for Ref with non-empty spine" in {
    val sem = Sem.Neut(Head.Ref(testFqn), Seq(Sem.Lit(Value.Direct(1, bigIntType))))
    Quoter.quote(sem) shouldBe None
  }

  it should "return None for Struct with unquotable field" in {
    val sem = Sem.Struct(testFqn, Map("A" -> Sem.Neut(Head.Param("x"), Seq.empty)))
    Quoter.quote(sem) shouldBe None
  }

  it should "round-trip bigIntType through Struct" in {
    val bigIntFqn = com.vanillasource.eliot.eliotc.eval.fact.Types.bigIntFQN
    val sem       = Sem.Neut(Head.Ref(bigIntFqn), Seq.empty)
    Quoter.quote(sem) shouldBe Some(bigIntType)
  }
}
