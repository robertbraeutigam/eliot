package com.vanillasource.eliot.eliotc.eval.fact

import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

class ExpressionValueTest extends AnyFlatSpec with Matchers {
  private val testModuleName = ModuleName(Seq.empty, "Test")
  private val intVfqn        = ValueFQN(testModuleName, QualifiedName("Int", Qualifier.Default))
  private val intType        = Types.dataType(intVfqn)

  private def s[T](value: T): Sourced[T] = Sourced(URI.create("Test.els"), PositionRange.zero, value)

  "stripLeadingLambdas" should "strip single FunctionLiteral" in {
    val inner = FunctionApplication(s(ParameterReference("A")), s(ParameterReference("A")))
    val sig   = FunctionLiteral("A", Value.Type, s(inner))
    ExpressionValue.stripLeadingLambdas(sig) shouldBe inner
  }

  it should "strip multiple FunctionLiterals" in {
    val inner = FunctionApplication(s(ParameterReference("A")), s(ParameterReference("B")))
    val sig   = FunctionLiteral("A", Value.Type, s(FunctionLiteral("B", Value.Type, s(inner))))
    ExpressionValue.stripLeadingLambdas(sig) shouldBe inner
  }

  it should "strip FunctionLiterals regardless of param type" in {
    val inner = ParameterReference("n")
    val sig   = FunctionLiteral("n", intType, s(inner))
    ExpressionValue.stripLeadingLambdas(sig) shouldBe inner
  }

  it should "return expression unchanged when no leading FunctionLiterals" in {
    val expr = ConcreteValue(intType)
    ExpressionValue.stripLeadingLambdas(expr) shouldBe expr
  }

  "extractLeadingLambdaParams" should "extract single parameter" in {
    val sig = FunctionLiteral("A", Value.Type, s(ParameterReference("A")))
    ExpressionValue.extractLeadingLambdaParams(sig) shouldBe Seq(("A", Value.Type))
  }

  it should "extract multiple parameters" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      s(FunctionLiteral("B", Value.Type, s(ParameterReference("A"))))
    )
    ExpressionValue.extractLeadingLambdaParams(sig) shouldBe Seq(("A", Value.Type), ("B", Value.Type))
  }

  it should "extract parameters with different types" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      s(FunctionLiteral("n", intType, s(ParameterReference("n"))))
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
      FunctionApplication(s(ParameterReference("x")), s(ParameterReference("x")))
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
    val body     = FunctionLiteral("y", intType, s(ParameterReference("x")))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    val result   = ExpressionValue.substitute(body, "x", argValue)
    result match {
      case FunctionLiteral("y", _, b) => b.value shouldBe argValue
      case other                      => fail(s"Unexpected result: $other")
    }
  }

  it should "not substitute in shadowed function literal" in {
    val body     = FunctionLiteral("x", intType, s(ParameterReference("x")))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe body
  }

  it should "leave concrete values unchanged" in {
    val body     = ConcreteValue(Value.Direct(99, intType))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe body
  }
}
