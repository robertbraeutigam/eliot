package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** The **woven re-check** (effects-as-channel v4 §6 / §9 standing rule 4 / §11 P3): the body leaving the codegen seam
  * is type-checked once more on ground types, so a machine-generated monadic core is never shipped unchecked.
  *
  * These pin what it rejects. That it accepts *today's* weave — the P3 gate, "land it on today's output first, where it
  * must be a no-op" — is pinned by the whole example and integration corpus, which runs through the seam.
  */
class WovenRecheckTest extends AnyFlatSpec with Matchers {
  private val uri = URI.create("Test.els")

  private def at[A](value: A): Sourced[A] = Sourced(uri, PositionRange.zero, value)

  private def typeOf(name: String): GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(Seq("eliot", "lang"), name), QualifiedName(name, Qualifier.Type)),
      Seq.empty,
      GroundValue.Type
    )

  private val string = typeOf("String")
  private val unit   = typeOf("Unit")

  private def fn(domain: GroundValue, codomain: GroundValue): GroundValue =
    GroundValue.Structure(WellKnownTypes.functionDataTypeFQN, Seq(domain, codomain), GroundValue.Type)

  private def node(t: GroundValue, e: MonomorphicExpression.Expression): Sourced[MonomorphicExpression] =
    at(MonomorphicExpression(t, e))

  private def literal(t: GroundValue): MonomorphicExpression.Expression =
    MonomorphicExpression.StringLiteral(at("s"))

  private def check(signature: GroundValue, body: MonomorphicExpression.Expression): Seq[WovenRecheck.Problem] =
    WovenRecheck.check(signature, Some(at(body)))

  "a well-typed lambda" should "re-check clean" in {
    check(fn(string, string), MonomorphicExpression.FunctionLiteral(at("x"), string, node(string, literal(string)))) shouldBe empty
  }

  "a lambda whose signature disagrees with its parameter type" should "be rejected" in {
    check(fn(unit, string), MonomorphicExpression.FunctionLiteral(at("x"), string, node(string, literal(string)))) should not be empty
  }

  "a lambda whose signature disagrees with its body type" should "be rejected" in {
    check(fn(string, unit), MonomorphicExpression.FunctionLiteral(at("x"), string, node(string, literal(string)))) should not be empty
  }

  "a parameter reference typed differently from its binder" should "be rejected — the shape a wrongly instantiated insertion takes" in {
    check(
      fn(string, string),
      MonomorphicExpression.FunctionLiteral(at("x"), string, node(string, MonomorphicExpression.ParameterReference(at("x"))))
    ) shouldBe empty
  }

  it should "be rejected when the reference's own type differs from the binder's" in {
    check(
      fn(string, unit),
      MonomorphicExpression.FunctionLiteral(at("x"), string, node(unit, MonomorphicExpression.ParameterReference(at("x"))))
    ) should not be empty
  }

  "an application of a non-function" should "be rejected" in {
    check(string, MonomorphicExpression.FunctionApplication(node(string, literal(string)), node(string, literal(string)))) should not be empty
  }

  "an application whose argument does not fit the domain" should "be rejected" in {
    check(
      string,
      MonomorphicExpression.FunctionApplication(node(fn(unit, string), literal(unit)), node(string, literal(string)))
    ) should not be empty
  }

  "an application whose recorded result type is not the codomain" should "be rejected" in {
    check(
      unit,
      MonomorphicExpression.FunctionApplication(node(fn(string, string), literal(string)), node(string, literal(string)))
    ) should not be empty
  }

  "a well-typed application" should "re-check clean" in {
    check(
      string,
      MonomorphicExpression.FunctionApplication(node(fn(string, string), literal(string)), node(string, literal(string)))
    ) shouldBe empty
  }

  "a body-less value" should "have nothing to re-check" in {
    WovenRecheck.check(string, None) shouldBe empty
  }
}
