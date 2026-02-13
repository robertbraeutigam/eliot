package com.vanillasource.eliot.eliotc.eval.fact

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExpressionValueTest extends AnyFlatSpec with Matchers {
  private val testModuleName = ModuleName(Seq.empty, "Test")
  private val intVfqn        = ValueFQN(testModuleName, QualifiedName("Int", Qualifier.Default))
  private val intType        = Types.dataType(intVfqn)

  "stripLeadingLambdas" should "strip single FunctionLiteral" in {
    val inner = ExpressionValue.functionType(ParameterReference("A", Value.Type), ParameterReference("A", Value.Type))
    val sig   = FunctionLiteral("A", Value.Type, inner)
    ExpressionValue.stripLeadingLambdas(sig) shouldBe inner
  }

  it should "strip multiple FunctionLiterals" in {
    val inner = ExpressionValue.functionType(ParameterReference("A", Value.Type), ParameterReference("B", Value.Type))
    val sig   = FunctionLiteral("A", Value.Type, FunctionLiteral("B", Value.Type, inner))
    ExpressionValue.stripLeadingLambdas(sig) shouldBe inner
  }

  it should "strip FunctionLiterals regardless of param type" in {
    val inner = ParameterReference("n", intType)
    val sig   = FunctionLiteral("n", intType, inner)
    ExpressionValue.stripLeadingLambdas(sig) shouldBe inner
  }

  it should "return expression unchanged when no leading FunctionLiterals" in {
    val expr = ConcreteValue(intType)
    ExpressionValue.stripLeadingLambdas(expr) shouldBe expr
  }

  "extractLeadingLambdaParams" should "extract single parameter" in {
    val sig = FunctionLiteral("A", Value.Type, ParameterReference("A", Value.Type))
    ExpressionValue.extractLeadingLambdaParams(sig) shouldBe Seq(("A", Value.Type))
  }

  it should "extract multiple parameters" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      FunctionLiteral("B", Value.Type, ParameterReference("A", Value.Type))
    )
    ExpressionValue.extractLeadingLambdaParams(sig) shouldBe Seq(("A", Value.Type), ("B", Value.Type))
  }

  it should "extract parameters with different types" in {
    val sig = FunctionLiteral(
      "A",
      Value.Type,
      FunctionLiteral("n", intType, ParameterReference("n", intType))
    )
    ExpressionValue.extractLeadingLambdaParams(sig) shouldBe Seq(("A", Value.Type), ("n", intType))
  }

  it should "return empty for non-parameterized expression" in {
    val expr = ConcreteValue(intType)
    ExpressionValue.extractLeadingLambdaParams(expr) shouldBe Seq.empty
  }

  "substitute" should "replace matching parameter reference" in {
    val body     = ParameterReference("x", intType)
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe argValue
  }

  it should "leave non-matching parameter reference unchanged" in {
    val body     = ParameterReference("y", intType)
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe body
  }

  it should "substitute in function application target and argument" in {
    val body     = FunctionApplication(ParameterReference("x", intType), ParameterReference("x", intType))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe FunctionApplication(argValue, argValue)
  }

  it should "substitute in function literal body when param name differs" in {
    val body     = FunctionLiteral("y", intType, ParameterReference("x", intType))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe FunctionLiteral("y", intType, argValue)
  }

  it should "not substitute in shadowed function literal" in {
    val body     = FunctionLiteral("x", intType, ParameterReference("x", intType))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe body
  }

  it should "leave concrete values unchanged" in {
    val body     = ConcreteValue(Value.Direct(99, intType))
    val argValue = ConcreteValue(Value.Direct(42, intType))
    ExpressionValue.substitute(body, "x", argValue) shouldBe body
  }
}
