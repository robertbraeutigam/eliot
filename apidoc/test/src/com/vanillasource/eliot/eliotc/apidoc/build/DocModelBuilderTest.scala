package com.vanillasource.eliot.eliotc.apidoc.build

import com.vanillasource.eliot.eliotc.apidoc.model.{DocItem, DocModule}
import com.vanillasource.eliot.eliotc.ast.fact.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

class DocModelBuilderTest extends AnyFlatSpec with Matchers {
  private val uri = URI.create("Test.els")

  private def s[A](a: A): Sourced[A]    = Sourced(uri, PositionRange.zero, a)
  private def ty(name: String, generics: Expression*): Expression =
    Expression.FunctionApplication(None, s(name), Option.when(generics.nonEmpty)(generics.map(s)), Seq.empty)
  private def gp(name: String): GenericParameter           = GenericParameter(s(name), s(ty("Type")), Seq.empty)
  private def arg(name: String, t: Expression)             = ArgumentDefinition(s(name), s(t))

  private def fn(
      name: String,
      qualifier: Qualifier = Qualifier.Default,
      generics: Seq[GenericParameter] = Seq.empty,
      args: Seq[ArgumentDefinition] = Seq.empty,
      ret: Expression = ty("Unit"),
      body: Option[Expression] = None,
      doc: Option[String] = None
  ): FunctionDefinition =
    FunctionDefinition(s(QualifiedName(name, qualifier)), generics, args, s(ret), body.map(s), doc = doc.map(s))

  private def ast(functions: Seq[FunctionDefinition] = Seq.empty, data: Seq[DataDefinition] = Seq.empty): AST =
    AST(Seq.empty, functions, data)

  private def moduleNamed(modules: Seq[DocModule], name: String): DocModule =
    modules.find(_.name.name == name).getOrElse(fail(s"module $name not generated"))

  "doc model builder" should "merge an abstract type with a platform data definition into one item" in {
    val ioModule = ModuleName(Seq("eliot", "lang"), "IO")
    val modules  = DocModelBuilder.build(
      Seq(
        (ioModule, "stdlib", ast(functions = Seq(fn("IO", Qualifier.Type, args = Seq(arg("A", ty("Type"))), doc = Some("the io"))))),
        (
          ioModule,
          "jvm",
          ast(data = Seq(DataDefinition(s("IO"), Seq(gp("A")), Some(Seq(DataConstructor(s("IO"), Seq(arg("block", ty("Function", ty("Unit"), ty("A"))))))))))
        )
      )
    )
    val item     = moduleNamed(modules, "IO").items.head

    item.kind shouldBe DocItem.Kind.TypeLike
    item.signature shouldBe "type IO[A]"
    item.doc shouldBe Some("the io")
    item.layers shouldBe Seq("stdlib", "jvm")
    item.implementedOn shouldBe Seq("jvm")
    item.members shouldBe Seq(DocItem.Member("data IO[A](block: Function[Unit, A])", None, Some("jvm")))
  }

  it should "merge an abstract def with its platform implementation, preferring the abstract signature and doc" in {
    val m       = ModuleName(Seq("eliot", "lang"), "Console")
    val modules = DocModelBuilder.build(
      Seq(
        (m, "stdlib", ast(functions = Seq(fn("println", args = Seq(arg("s", ty("String"))), ret = ty("IO", ty("Unit")), doc = Some("print a line"))))),
        (m, "jvm", ast(functions = Seq(fn("println", args = Seq(arg("s", ty("String"))), ret = ty("IO", ty("Unit")), body = Some(ty("io"))))))
      )
    )
    val item    = moduleNamed(modules, "Console").items.head

    item.kind shouldBe DocItem.Kind.Value
    item.signature shouldBe "def println(s: String): IO[Unit]"
    item.doc shouldBe Some("print a line")
    item.implementedOn shouldBe Seq("jvm")
  }

  it should "gather ability implementations from across modules under the ability" in {
    val showModule = ModuleName(Seq("eliot", "lang"), "Show")
    val userModule = ModuleName(Seq.empty, "App")
    val modules    = DocModelBuilder.build(
      Seq(
        (
          showModule,
          "stdlib",
          ast(functions =
            Seq(
              fn("Show", Qualifier.Ability("Show"), generics = Seq(gp("A"))),
              fn("show", Qualifier.Ability("Show"), generics = Seq(gp("A")), args = Seq(arg("a", ty("A"))), ret = ty("String"))
            )
          )
        ),
        (userModule, "src", ast(functions = Seq(fn("Show", Qualifier.AbilityImplementation(s("Show"), 0), args = Seq(arg("arg0", ty("Hello")))))))
      )
    )
    val item       = moduleNamed(modules, "Show").items.find(_.kind == DocItem.Kind.Ability).getOrElse(fail("no ability item"))

    item.signature shouldBe "ability Show[A]"
    item.members.map(_.signature) shouldBe Seq("def show(a: A): String")
    item.implementations shouldBe Seq(DocItem.Implementation("implement Show[Hello]", Seq("src"), None))
  }
}
