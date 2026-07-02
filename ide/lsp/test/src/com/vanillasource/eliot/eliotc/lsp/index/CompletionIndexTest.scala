package com.vanillasource.eliot.eliotc.lsp.index

import com.vanillasource.eliot.eliotc.core.fact.{Expression as CoreExpression, NamedValue, TypeStack}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ModuleValue, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, QualifiedName as ResolvedQualifiedName, Qualifier as ResolvedQualifier, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.net.URI

class CompletionIndexTest extends AnyFlatSpec with Matchers {
  private val module = ModuleName(Seq.empty, "Test")
  private val uri    = new File("/tmp/Test.els").toURI
  private val range  = PositionRange(Position(1, 1), Position(1, 1))

  // One file's dictionary: a plain value with a known signature, a data type (type + value constructor of the same
  // name), an ability method, a synthetic ability-implementation marker, and an in-scope name with no resolved value.
  private val dictionary = Map(
    qn("printLine", Qualifier.Default)                                         -> fqn("printLine"),
    qn("Counter", Qualifier.Type)                                            -> fqn("Counter"),
    qn("Counter", Qualifier.Default)                                         -> fqn("Counter"),
    qn("combine", Qualifier.Ability("Combine"))                              -> fqn("combine"),
    qn("Combine", Qualifier.AbilityImplementation(Sourced(uri, range, "Combine"), 0)) -> fqn("Combine"),
    qn("ambientOnly", Qualifier.Default)                                     -> fqn("ambientOnly")
  )
  private val index      = CompletionIndex.build(Seq(moduleValue(dictionary)), Seq(resolved("printLine", "String")))

  "completionsAt" should "offer every in-scope name once by its bare written name, dropping the synthetic marker" in {
    index.completionsAt(uri).map(_.name) shouldBe Seq("Counter", "ambientOnly", "combine", "printLine")
  }

  it should "classify a name with a type-namespace qualifier as a type" in {
    index.completionsAt(uri).find(_.name == "Counter").map(_.kind) shouldBe Some(CompletionIndex.Kind.Type)
  }

  it should "classify an ability member as an ability method" in {
    index.completionsAt(uri).find(_.name == "combine").map(_.kind) shouldBe Some(CompletionIndex.Kind.AbilityMethod)
  }

  it should "classify a plain value as a value" in {
    index.completionsAt(uri).find(_.name == "printLine").map(_.kind) shouldBe Some(CompletionIndex.Kind.Value)
  }

  it should "render the signature of a name whose resolved value was materialised" in {
    index.completionsAt(uri).find(_.name == "printLine").flatMap(_.detail) shouldBe Some("Test::String")
  }

  it should "have no detail for an in-scope name with no resolved value" in {
    index.completionsAt(uri).find(_.name == "ambientOnly").flatMap(_.detail) shouldBe None
  }

  it should "return nothing for a document that has no in-scope names" in {
    index.completionsAt(new File("/tmp/Other.els").toURI) shouldBe empty
  }

  it should "match the editor's file:/// URI form against the compiler's file:/ form" in {
    index.completionsAt(URI.create("file:///tmp/Test.els")).map(_.name) should contain("printLine")
  }

  private def qn(name: String, qualifier: Qualifier): QualifiedName = QualifiedName(name, qualifier)

  private def fqn(name: String): ValueFQN = ValueFQN(module, QualifiedName(name, Qualifier.Default))

  private def moduleValue(dictionary: Map[QualifiedName, ValueFQN]): ModuleValue =
    ModuleValue(uri, fqn("module"), dictionary, placeholderNamedValue)

  private val placeholderNamedValue: NamedValue =
    NamedValue(
      Sourced(uri, range, QualifiedName("module", Qualifier.Default)),
      None,
      TypeStack.of(CoreExpression.StringLiteral(Sourced(uri, range, "")))
    )

  /** A resolved value whose signature is a reference to `signatureName` (so its rendered detail is `Test::signatureName`).
    */
  private def resolved(name: String, signatureName: String): ResolvedValue =
    ResolvedValue(
      vfqn = fqn(name),
      name = Sourced(uri, range, ResolvedQualifiedName(name, ResolvedQualifier.Default)),
      runtime = None,
      typeStack = Sourced(uri, range, TypeStack.of(Expression.ValueReference(Sourced(uri, range, fqn(signatureName)))))
    )
}
