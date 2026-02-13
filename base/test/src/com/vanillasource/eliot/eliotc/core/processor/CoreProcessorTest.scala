package com.vanillasource.eliot.eliotc.core.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.{CoreAST, Expression, QualifiedName, Qualifier, TypeStack, NamedValue}
import com.vanillasource.eliot.eliotc.token.Tokenizer

class CoreProcessorTest extends ProcessorTest(Tokenizer(), ASTParser(), CoreProcessor()) {

  "core processor" should "transform a simple constant reference" in {
    namedValue("a: A = b", QualifiedName("a", Qualifier.Default)).asserting { nv =>
      (nv.qualifiedName.value.name, nv.typeStack.signatureStructure, nv.runtimeStructure) shouldBe
        ("a", Ref("A"), Some(Ref("b")))
    }
  }

  it should "transform an abstract constant without body" in {
    namedValue("a: A", QualifiedName("a", Qualifier.Default)).asserting { nv =>
      (nv.qualifiedName.value.name, nv.typeStack.signatureStructure, nv.runtime) shouldBe ("a", Ref("A"), None)
    }
  }

  it should "place curried function type in typeStack for function without generics" in {
    namedValue("f(x: X): R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe App(App(Ref("Function"), Ref("X")), Ref("R"))
    }
  }

  it should "transform a single-parameter function body into lambda" in {
    namedValue("f(x: X): R = y").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("x", Ref("X"), Ref("y")))
    }
  }

  it should "place curried function type in typeStack for multi-parameter function" in {
    namedValue("f(x: X, y: Y): R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe
        App(App(Ref("Function"), Ref("X")), App(App(Ref("Function"), Ref("Y")), Ref("R")))
    }
  }

  it should "transform a two-parameter function body into nested lambdas" in {
    namedValue("f(x: X, y: Y): R = z").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("x", Ref("X"), Lambda("y", Ref("Y"), Ref("z"))))
    }
  }

  it should "transform three-parameter function body into deeply nested lambdas" in {
    namedValue("f(a: A, b: B, c: C): R = r").asserting { nv =>
      nv.runtimeStructure shouldBe
        Some(Lambda("a", Ref("A"), Lambda("b", Ref("B"), Lambda("c", Ref("C"), Ref("r")))))
    }
  }

  "generic parameters" should "become outer lambdas with Type reference in typeStack" in {
    namedValue("f[A]: R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe Lambda("A", Ref("Type"), Ref("R"))
    }
  }

  it should "preserve order for multiple generic parameters" in {
    namedValue("f[A, B]: R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe Lambda("A", Ref("Type"), Lambda("B", Ref("Type"), Ref("R")))
    }
  }

  it should "place generics then function args in typeStack" in {
    namedValue("f[A](x: X): R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe Lambda("A", Ref("Type"), App(App(Ref("Function"), Ref("X")), Ref("R")))
    }
  }

  it should "keep function args only in the value" in {
    namedValue("f[A](x: X): R = y").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("x", Ref("X"), Ref("y")))
    }
  }

  it should "handle multiple generics with multiple parameters" in {
    namedValue("f[A, B](x: X, y: Y): R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe
        Lambda(
          "A",
          Ref("Type"),
          Lambda("B", Ref("Type"), App(App(Ref("Function"), Ref("X")), App(App(Ref("Function"), Ref("Y")), Ref("R"))))
        )
    }
  }

  it should "nest function args in value for generic functions" in {
    namedValue("f[A, B](x: X, y: Y): R = z").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("x", Ref("X"), Lambda("y", Ref("Y"), Ref("z"))))
    }
  }

  "type references with generics" should "convert to function applications" in {
    namedValue("f: A[B]").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe App(Ref("A"), Ref("B"))
    }
  }

  it should "convert two-parameter type references to chained applications" in {
    namedValue("f: A[B, C]").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe App(App(Ref("A"), Ref("B")), Ref("C"))
    }
  }

  it should "convert deeply nested type references" in {
    namedValue("f: A[B[C]]").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe App(Ref("A"), App(Ref("B"), Ref("C")))
    }
  }

  "function applications" should "convert simple call to function application" in {
    namedValue("f: R = g(a)").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(Ref("g"), Ref("a")))
    }
  }

  it should "curry multi-argument calls" in {
    namedValue("f: R = g(a, b)").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(App(Ref("g"), Ref("a")), Ref("b")))
    }
  }

  it should "curry three-argument calls" in {
    namedValue("f: R = g(a, b, c)").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(App(App(Ref("g"), Ref("a")), Ref("b")), Ref("c")))
    }
  }

  it should "handle nested function calls" in {
    namedValue("f: R = g(h(a))").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(Ref("g"), App(Ref("h"), Ref("a"))))
    }
  }

  "qualified function applications" should "preserve module qualifier" in {
    namedValue("f: R = mod.sub.Mod::func").asserting { nv =>
      nv.runtimeStructure shouldBe Some(QualRef("func", "mod.sub.Mod"))
    }
  }

  it should "curry qualified calls with arguments" in {
    namedValue("f: R = Mod::func(a)").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(QualRef("func", "Mod"), Ref("a")))
    }
  }

  "lambda expressions" should "convert single-parameter lambda" in {
    namedValue("f: R = (a: A) -> b").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("a", Ref("A"), Ref("b")))
    }
  }

  it should "curry multi-parameter lambda" in {
    namedValue("f: R = (a: A, b: B) -> c").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("a", Ref("A"), Lambda("b", Ref("B"), Ref("c"))))
    }
  }

  it should "handle lambda without parentheses" in {
    namedValue("f: R = a: A -> b").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("a", Ref("A"), Ref("b")))
    }
  }

  it should "handle nested lambdas" in {
    namedValue("f: R = (a: A) -> (b: B) -> c").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("a", Ref("A"), Lambda("b", Ref("B"), Ref("c"))))
    }
  }

  "literals" should "convert integer literals" in {
    namedValue("f: R = 42").asserting { nv =>
      nv.runtimeStructure shouldBe Some(IntLit("42"))
    }
  }

  it should "convert string literals without quotes" in {
    namedValue("f: R = \"hello\"").asserting { nv =>
      nv.runtimeStructure shouldBe Some(StrLit("hello"))
    }
  }

  "data definitions" should "generate type function with Type qualifier" in {
    namedValues("data Person").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should contain(QualifiedName("Person", Qualifier.Type))
    }
  }

  it should "generate type function returning Type" in {
    namedValue("data Person", QualifiedName("Person", Qualifier.Type)).asserting { nv =>
      nv.typeStack.signatureStructure shouldBe Ref("Type")
    }
  }

  // Note: Signature uses FunctionLiteral to preserve parameter names (A: Type -> Type)
  it should "generate type function with generic param and argument in typeStack" in {
    namedValue("data Box[A]", QualifiedName("Box", Qualifier.Type)).asserting { nv =>
      nv.typeStack.signatureStructure shouldBe Lambda("A", Ref("Type"), Ref("Type"))
    }
  }

  it should "generate constructor for data with fields" in {
    namedValues("data Person(name: String)").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should contain(QualifiedName("Person", Qualifier.Default))
    }
  }

  it should "generate constructor with curried arguments in typeStack" in {
    namedValue("data Person(name: Name, age: Age)", QualifiedName("Person", Qualifier.Default)).asserting { nv =>
      nv.typeStack.signatureStructure shouldBe
        App(App(Ref("Function"), Ref("Name")), App(App(Ref("Function"), Ref("Age")), Ref("Person", Qualifier.Type)))
    }
  }

  it should "not generate constructor for abstract data without fields" in {
    namedValues("data Abstract").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should not contain QualifiedName("Abstract", Qualifier.Default)
      nvs.map(_.qualifiedName.value) should contain(QualifiedName("Abstract", Qualifier.Type))
    }
  }

  it should "generate accessor functions for each field" in {
    namedValues("data Person(name: Name, age: Age)").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should contain allOf (QualifiedName("name", Qualifier.Default), QualifiedName("age", Qualifier.Default))
    }
  }

  it should "generate accessor with argument in typeStack" in {
    namedValue("data Person(name: Name)", QualifiedName("name", Qualifier.Default)).asserting { nv =>
      nv.typeStack.signatureStructure shouldBe App(App(Ref("Function"), Ref("Person", Qualifier.Type)), Ref("Name"))
    }
  }

  it should "generate accessor with generic param and argument in typeStack" in {
    namedValue("data Box[A](value: A)", QualifiedName("value", Qualifier.Default)).asserting { nv =>
      // The accessor type is (A :: Type) -> Function(Box^Type(A), A)
      nv.typeStack.signatureStructure shouldBe Lambda(
        "A",
        Ref("Type"),
        App(App(Ref("Function"), App(Ref("Box", Qualifier.Type), Ref("A"))), Ref("A"))
      )
    }
  }

  "complex scenarios" should "handle mixed functions and data" in {
    runEngineForCoreAST("data A\nf: A").asserting { ast =>
      ast.namedValues.map(_.qualifiedName.value) should contain allOf (QualifiedName("A", Qualifier.Type), QualifiedName("f", Qualifier.Default))
    }
  }

  it should "handle data with generic constructor" in {
    namedValue("data Pair[A, B](fst: A, snd: B)", QualifiedName("Pair", Qualifier.Default)).asserting { nv =>
      // The constructor type is (A :: Type) -> (B :: Type) -> Function(A, Function(B, Pair^Type(A)(B)))
      nv.typeStack.signatureStructure shouldBe
        Lambda(
          "A",
          Ref("Type"),
          Lambda(
            "B",
            Ref("Type"),
            App(
              App(Ref("Function"), Ref("A")),
              App(App(Ref("Function"), Ref("B")), App(App(Ref("Pair", Qualifier.Type), Ref("A")), Ref("B")))
            )
          )
        )
    }
  }

  it should "preserve import statements in core AST" in {
    runEngineForCoreAST("import a.b.C\nf: R").asserting { ast =>
      ast.importStatements.map(i => (i.packageNames.map(_.value) :+ i.moduleName.value).mkString(".")) shouldBe
        Seq("a.b.C")
    }
  }

  // Structural representation of expressions (ignores source positions)
  sealed trait ExprStructure
  case class Ref(name: String, qualifier: Qualifier = Qualifier.Default)           extends ExprStructure
  case class QualRef(name: String, module: String)                                extends ExprStructure
  case class App(target: ExprStructure, arg: ExprStructure)                       extends ExprStructure
  case class Lambda(param: String, paramType: ExprStructure, body: ExprStructure) extends ExprStructure
  case class IntLit(value: String)                                                extends ExprStructure
  case class StrLit(value: String)                                                extends ExprStructure
  case object Empty                                                               extends ExprStructure

  extension (nv: NamedValue) {
    def runtimeStructure: Option[ExprStructure] = nv.runtime.map(_.structure)
  }

  extension (stack: TypeStack[Expression]) {
    def signatureStructure: ExprStructure = stack.signature.structure
    // Gets the first expression in the stack (signature for TypeStack)
    def firstStructure: ExprStructure     = stack.levels.head.structure
  }

  extension (expr: Expression) {
    def structure: ExprStructure = expr match {
      case NamedValueReference(name, None)         => Ref(name.value.name, name.value.qualifier)
      case NamedValueReference(name, Some(qual))   => QualRef(name.value.name, qual.value)
      case FunctionApplication(target, arg)        => App(target.value.firstStructure, arg.value.firstStructure)
      case FunctionLiteral(param, paramType, body) =>
        Lambda(param.value, paramType.firstStructure, body.value.firstStructure)
      case IntegerLiteral(lit)                     => IntLit(lit.value)
      case StringLiteral(lit)                      => StrLit(lit.value)
    }
  }

  private def namedValue(source: String, name: QualifiedName = QualifiedName("f", Qualifier.Default)): IO[NamedValue] =
    runEngineForCoreAST(source).map(_.namedValues.find(_.qualifiedName.value == name).get)

  private def namedValues(source: String): IO[Seq[NamedValue]] =
    runEngineForCoreAST(source).map(_.namedValues)

  private def runEngineForCoreAST(source: String): IO[com.vanillasource.eliot.eliotc.core.fact.AST] =
    runGenerator(source, CoreAST.Key(file)).flatMap { case (errors, facts) =>
      if (errors.nonEmpty) {
        IO.raiseError(new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}"))
      } else {
        facts.values.collectFirst { case CoreAST(_, ast) => ast.value } match {
          case Some(ast) => IO.pure(ast)
          case None      => IO.raiseError(new Exception("No CoreAST found"))
        }
      }
    }
}
