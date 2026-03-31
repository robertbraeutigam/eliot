package com.vanillasource.eliot.eliotc.eval.fact

import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExpressionValueTest extends AnyFlatSpec with Matchers {
  private val testModuleName = ModuleName(Seq.empty, "Test")
  private val intVfqn        = ValueFQN(testModuleName, QualifiedName("Int", Qualifier.Default))
  private val intType        = Types.dataType(intVfqn)

  "stripLeadingLambdas" should "strip single FunctionLiteral" in {
    val inner = FunctionApplication(unsourced(ParameterReference("A")), unsourced(ParameterReference("A")))
    val sig   = FunctionLiteral("A", Value.Type, unsourced(inner))
    ExpressionValue.stripLeadingLambdas(sig) shouldBe inner
  }

  it should "strip multiple FunctionLiterals" in {
    val inner = FunctionApplication(unsourced(ParameterReference("A")), unsourced(ParameterReference("B")))
    val sig   = FunctionLiteral("A", Value.Type, unsourced(FunctionLiteral("B", Value.Type, unsourced(inner))))
    ExpressionValue.stripLeadingLambdas(sig) shouldBe inner
  }

  it should "strip FunctionLiterals regardless of param type" in {
    val inner = ParameterReference("n")
    val sig   = FunctionLiteral("n", intType, unsourced(inner))
    ExpressionValue.stripLeadingLambdas(sig) shouldBe inner
  }

  it should "return expression unchanged when no leading FunctionLiterals" in {
    val expr = ConcreteValue(intType)
    ExpressionValue.stripLeadingLambdas(expr) shouldBe expr
  }

  "extractLeadingLambdaParams" should "extract single parameter" in {
    val sig = FunctionLiteral("A", Value.Type, unsourced(ParameterReference("A")))
    ExpressionValue.extractLeadingLambdaParams(sig) shouldBe Seq(("A", Value.Type))
  }

  it should "extract multiple parameters" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      unsourced(FunctionLiteral("B", Value.Type, unsourced(ParameterReference("A"))))
    )
    ExpressionValue.extractLeadingLambdaParams(sig) shouldBe Seq(("A", Value.Type), ("B", Value.Type))
  }

  it should "extract parameters with different types" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      unsourced(FunctionLiteral("n", intType, unsourced(ParameterReference("n"))))
    )
    ExpressionValue.extractLeadingLambdaParams(sig) shouldBe Seq(("A", Value.Type), ("n", intType))
  }

  it should "return empty for non-parameterized expression" in {
    val expr = ConcreteValue(intType)
    ExpressionValue.extractLeadingLambdaParams(expr) shouldBe Seq.empty
  }

  "substitute" should "replace matching parameter reference" in {
    val body     = ParameterReference("x")
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe argValue
  }

  it should "leave non-matching parameter reference unchanged" in {
    val body     = ParameterReference("y")
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe body
  }

  it should "substitute in function application target and argument" in {
    val body     =
      FunctionApplication(unsourced(ParameterReference("x")), unsourced(ParameterReference("x")))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    val result   = ExpressionValue.substitute(body, "x", argValue)
    result match {
      case FunctionApplication(t, a) =>
        t.value shouldBe argValue
        a.value shouldBe argValue
      case other                     => fail(s"Unexpected result: $other")
    }
  }

  it should "substitute in function literal body when param name differs" in {
    val body     = FunctionLiteral("y", intType, unsourced(ParameterReference("x")))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    val result   = ExpressionValue.substitute(body, "x", argValue)
    result match {
      case FunctionLiteral("y", _, b) => b.value shouldBe argValue
      case other                      => fail(s"Unexpected result: $other")
    }
  }

  it should "not substitute in shadowed function literal" in {
    val body     = FunctionLiteral("x", intType, unsourced(ParameterReference("x")))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe body
  }

  it should "leave concrete values unchanged" in {
    val body     = ConcreteValue(Value.Direct(99, intType))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe body
  }
}
