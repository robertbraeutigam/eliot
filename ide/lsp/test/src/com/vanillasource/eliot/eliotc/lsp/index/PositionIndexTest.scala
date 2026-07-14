package com.vanillasource.eliot.eliotc.lsp.index

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName as ModuleQualifiedName, Qualifier as ModuleQualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, QualifiedName, Qualifier, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.net.URI

class PositionIndexTest extends AnyFlatSpec with Matchers {
  private val module = ModuleName(Seq.empty, "Test")
  private val uri    = new File("/tmp/Test.els").toURI

  // `foo` and `qux` are defined; `bar`'s body applies `foo` to `qux`, so it references both on line 3.
  private val foo   = definition("foo", at(5, 1, 5, 4))
  private val qux   = definition("qux", at(6, 1, 6, 4))
  private val bar   = definition(
    "bar",
    at(1, 1, 1, 4),
    body = Some(
      Expression.FunctionApplication(
        sourced(at(3, 5, 3, 8), reference("foo", at(3, 5, 3, 8))),
        sourced(at(3, 9, 3, 12), reference("qux", at(3, 9, 3, 12)))
      )
    )
  )
  private val index = PositionIndex.build(Seq(foo, qux, bar))

  "referenceAt" should "find the value reference at a position" in {
    index.referenceAt(uri, Position(3, 6)).map(_.value) shouldBe Some(fqn("foo"))
  }

  it should "find the adjacent reference at a different position" in {
    index.referenceAt(uri, Position(3, 10)).map(_.value) shouldBe Some(fqn("qux"))
  }

  it should "return None where no reference is present" in {
    index.referenceAt(uri, Position(4, 1)) shouldBe None
  }

  it should "prefer the most specific reference when ranges overlap" in {
    val outer = definition("outerHost", at(1, 1, 1, 4), body = Some(reference("outer", at(10, 1, 10, 20))))
    val inner = definition("innerHost", at(2, 1, 2, 4), body = Some(reference("inner", at(10, 5, 10, 8))))
    PositionIndex.build(Seq(outer, inner)).referenceAt(uri, Position(10, 6)).map(_.value) shouldBe Some(fqn("inner"))
  }

  it should "match the editor's file:/// URI form against the compiler's file:/ form" in {
    index.referenceAt(URI.create("file:///tmp/Test.els"), Position(3, 6)).map(_.value) shouldBe Some(fqn("foo"))
  }

  "definitionAt" should "resolve a reference to the defining name's location" in {
    index.definitionAt(uri, Position(3, 6)) shouldBe Some(sourced(at(5, 1, 5, 4), QualifiedName("foo", Qualifier.Default)))
  }

  it should "return None for a reference whose target is not in the workspace" in {
    val orphan = definition("orphanHost", at(1, 1, 1, 4), body = Some(reference("missing", at(7, 1, 7, 8))))
    PositionIndex.build(Seq(orphan)).definitionAt(uri, Position(7, 3)) shouldBe None
  }

  "hoverAt" should "return the reference together with the value it resolves to" in {
    index.hoverAt(uri, Position(3, 6)).map { case (reference, value) => (reference.value, value.vfqn) } shouldBe
      Some((fqn("foo"), fqn("foo")))
  }

  private def fqn(name: String): ValueFQN = ValueFQN(module, ModuleQualifiedName(name, ModuleQualifier.Default))

  private def at(fromLine: Int, fromCol: Int, toLine: Int, toCol: Int): PositionRange =
    PositionRange(Position(fromLine, fromCol), Position(toLine, toCol))

  private def sourced[A](range: PositionRange, value: A): Sourced[A] = Sourced(uri, range, value)

  private def reference(name: String, range: PositionRange): Expression =
    Expression.ValueReference(sourced(range, fqn(name)))

  /** A resolved value with a given definition-name location, an inert (FQN-free) signature, and an optional body. */
  private def definition(name: String, nameRange: PositionRange, body: Option[Expression] = None): ResolvedValue =
    ResolvedValue(
      vfqn = fqn(name),
      name = sourced(nameRange, QualifiedName(name, Qualifier.Default)),
      runtime = body.map(expression => sourced(nameRange, expression)),
      signature = sourced(nameRange, Expression.ParameterReference(sourced(at(0, 0, 0, 0), "T")))
    )
}
