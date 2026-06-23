package com.vanillasource.eliot.eliotc.core.processor

import com.vanillasource.eliot.eliotc.ast.fact.{ArgumentDefinition, DataConstructor, DataDefinition, Expression}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** M0 precondition #2: strict positivity rejects a data type that names its own type constructor in a contravariant
  * (left-of-arrow) position, the declared route to the Y-combinator's `x x`.
  */
class StrictPositivityCheckerTest extends AnyFlatSpec with Matchers {
  private val uri: URI = URI.create("Test.els")

  private def s[T](value: T): Sourced[T] = Sourced(uri, PositionRange.zero, value)

  private def ref(name: String, genericArgs: Seq[Sourced[Expression]] = Seq.empty): Sourced[Expression] =
    s(Expression.FunctionApplication(None, s(name), Option.when(genericArgs.nonEmpty)(genericArgs), Seq.empty))

  private def function(domain: Sourced[Expression], codomain: Sourced[Expression]): Sourced[Expression] =
    ref("Function", Seq(domain, codomain))

  /** `data <name>(<field>: <fieldType>)` with a single one-field constructor. */
  private def dataWith(name: String, fieldType: Sourced[Expression]): DataDefinition =
    DataDefinition(
      s(name),
      Seq.empty,
      Some(Seq(DataConstructor(s(name), Seq(ArgumentDefinition(s("field"), fieldType)))))
    )

  "data Loop(f: Function[Loop, A])" should "be rejected as not strictly positive" in {
    StrictPositivityChecker.check(dataWith("Loop", function(ref("Loop"), ref("A")))).map(_.value) shouldBe
      List("Recursive type 'Loop' may not appear in a contravariant position (left of '->').")
  }

  "data Box(content: Function[A, B])" should "be accepted (no self-reference)" in {
    StrictPositivityChecker.check(dataWith("Box", function(ref("A"), ref("B")))) shouldBe empty
  }

  "data Tree(left: Tree, right: Tree)" should "be accepted (covariant self-reference is structural)" in {
    val tree = DataDefinition(
      s("Tree"),
      Seq.empty,
      Some(Seq(DataConstructor(s("Tree"), Seq(ArgumentDefinition(s("left"), ref("Tree")), ArgumentDefinition(s("right"), ref("Tree"))))))
    )
    StrictPositivityChecker.check(tree) shouldBe empty
  }

  "the codomain of a function field" should "stay covariant (self-reference in the result is allowed)" in {
    StrictPositivityChecker.check(dataWith("Stream", function(ref("Unit"), ref("Stream")))) shouldBe empty
  }

  "a doubly-negated self-reference (Function[Function[Loop, A], B])" should "be accepted (positive again)" in {
    StrictPositivityChecker.check(dataWith("Loop", function(function(ref("Loop"), ref("A")), ref("B")))) shouldBe empty
  }

  "a self-reference nested under another constructor in a domain" should "be rejected" in {
    StrictPositivityChecker.check(dataWith("Loop", function(ref("List", Seq(ref("Loop"))), ref("A")))).map(_.value) shouldBe
      List("Recursive type 'Loop' may not appear in a contravariant position (left of '->').")
  }
}
