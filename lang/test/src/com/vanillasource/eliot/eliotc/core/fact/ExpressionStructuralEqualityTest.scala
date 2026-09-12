package com.vanillasource.eliot.eliotc.core.fact

import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** [[Expression.structuralEquality]] is what the layer merge compares two copies of a signature with
  * (`UnifiedModuleValueProcessor.unifyValues`), so every expression shape a *signature* can contain needs an arm here:
  * an unmatched pair falls to `false` and the merge reports "Has multiple different definitions." for two copies that
  * agree.
  *
  * The shape under test is a slot's `with` (`obj: {Abort} A with abortByEscape`), which had no arm — the reason a
  * layer could not body a discharger whose slot names an implementation.
  */
class ExpressionStructuralEqualityTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  "structural equality of a slot `with`" should "hold for two copies from different layers" in {
    Expression.structuralEquality.eqv(withBinding("abortByEscape"), withBinding("abortByEscape")) shouldBe true
  }

  it should "not hold when the copies name different implementations" in {
    Expression.structuralEquality.eqv(withBinding("abortByEscape"), withBinding("abortByCell")) shouldBe false
  }

  it should "not hold when the copies name different module qualifiers" in {
    Expression.structuralEquality
      .eqv(withBinding("run", Some("Abort")), withBinding("run", Some("Throw"))) shouldBe false
  }

  it should "not hold against the same subject with no `with` at all" in {
    Expression.structuralEquality.eqv(withBinding("abortByEscape"), subject) shouldBe false
  }

  /** A `with` over [[subject]], built at a position of its own — two layers' copies never share one, and positions must
    * not contribute.
    */
  private def withBinding(implementationName: String, moduleName: Option[String] = None): Expression =
    Expression.WithBinding(
      at(subject),
      at(implementationName),
      moduleName.map(at)
    )

  private val subject: Expression = Expression.NamedValueReference(at(QualifiedName("A", Qualifier.Default)), None, Seq.empty)

  private var nextLine = 1

  private def at[T](value: T): Sourced[T] = {
    nextLine = nextLine + 1
    Sourced(URI.create(s"file:/layer$nextLine.els"), PositionRange(Position(nextLine, 1), Position(nextLine, 9)), value)
  }
}
