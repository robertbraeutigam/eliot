package com.vanillasource.eliot.eliotc.core.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.ast.fact.Fixity
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.{CoreAST, Expression, TypeStack, NamedValue}
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.token.Tokenizer

class CoreProcessorTest extends ProcessorTest(Tokenizer(), ASTParser(), CoreProcessor()) {
  private val T = Qualifier.Type

  "core processor" should "transform a simple constant reference" in {
    namedValue("def a: A = b", QualifiedName("a", Qualifier.Default)).asserting { nv =>
      (nv.qualifiedName.value.name, nv.typeStack.signatureStructure, nv.runtimeStructure) shouldBe
        ("a", Ref("A", T), Some(Ref("b")))
    }
  }

  it should "transform an abstract constant without body" in {
    namedValue("def a: A", QualifiedName("a", Qualifier.Default)).asserting { nv =>
      (nv.qualifiedName.value.name, nv.typeStack.signatureStructure, nv.runtime) shouldBe ("a", Ref("A", T), None)
    }
  }

  it should "place curried function type in typeStack for function without generics" in {
    namedValue("def f(x: X): R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe App(App(Ref("Function", T), Ref("X", T)), Ref("R", T))
    }
  }

  it should "transform a single-parameter function body into lambda" in {
    namedValue("def f(x: X): R = y").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("x", Ref("X", T), Ref("y")))
    }
  }

  it should "place curried function type in typeStack for multi-parameter function" in {
    namedValue("def f(x: X, y: Y): R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe
        App(App(Ref("Function", T), Ref("X", T)), App(App(Ref("Function", T), Ref("Y", T)), Ref("R", T)))
    }
  }

  it should "transform a two-parameter function body into nested lambdas" in {
    namedValue("def f(x: X, y: Y): R = z").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("x", Ref("X", T), Lambda("y", Ref("Y", T), Ref("z"))))
    }
  }

  it should "transform three-parameter function body into deeply nested lambdas" in {
    namedValue("def f(a: A, b: B, c: C): R = r").asserting { nv =>
      nv.runtimeStructure shouldBe
        Some(Lambda("a", Ref("A", T), Lambda("b", Ref("B", T), Lambda("c", Ref("C", T), Ref("r")))))
    }
  }

  "generic parameters" should "become outer lambdas with Type reference in typeStack" in {
    namedValue("def f[A]: R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe Lambda("A", Ref("Type"), Ref("R", T))
    }
  }

  it should "preserve order for multiple generic parameters" in {
    namedValue("def f[A, B]: R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe Lambda("A", Ref("Type"), Lambda("B", Ref("Type"), Ref("R", T)))
    }
  }

  it should "place generics then function args in typeStack" in {
    namedValue("def f[A](x: X): R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe
        Lambda("A", Ref("Type"), App(App(Ref("Function", T), Ref("X", T)), Ref("R", T)))
    }
  }

  it should "keep function args only in the value" in {
    namedValue("def f[A](x: X): R = y").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("x", Ref("X", T), Ref("y")))
    }
  }

  it should "handle multiple generics with multiple parameters" in {
    namedValue("def f[A, B](x: X, y: Y): R").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe
        Lambda(
          "A",
          Ref("Type"),
          Lambda(
            "B",
            Ref("Type"),
            App(App(Ref("Function", T), Ref("X", T)), App(App(Ref("Function", T), Ref("Y", T)), Ref("R", T)))
          )
        )
    }
  }

  it should "nest function args in value for generic functions" in {
    namedValue("def f[A, B](x: X, y: Y): R = z").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("x", Ref("X", T), Lambda("y", Ref("Y", T), Ref("z"))))
    }
  }

  "type references with generics" should "convert to function applications" in {
    namedValue("def f: A[B]").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe App(Ref("A", T), Ref("B", T))
    }
  }

  it should "convert two-parameter type references to chained applications" in {
    namedValue("def f: A[B, C]").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe App(App(Ref("A", T), Ref("B", T)), Ref("C", T))
    }
  }

  it should "convert deeply nested type references" in {
    namedValue("def f: A[B[C]]").asserting { nv =>
      nv.typeStack.signatureStructure shouldBe App(Ref("A", T), App(Ref("B", T), Ref("C", T)))
    }
  }

  "function applications" should "convert simple call to function application" in {
    namedValue("def f: R = g(a)").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(Ref("g"), Ref("a")))
    }
  }

  it should "curry multi-argument calls" in {
    namedValue("def f: R = g(a, b)").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(App(Ref("g"), Ref("a")), Ref("b")))
    }
  }

  it should "curry three-argument calls" in {
    namedValue("def f: R = g(a, b, c)").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(App(App(Ref("g"), Ref("a")), Ref("b")), Ref("c")))
    }
  }

  it should "handle nested function calls" in {
    namedValue("def f: R = g(h(a))").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(Ref("g"), App(Ref("h"), Ref("a"))))
    }
  }

  "qualified function applications" should "preserve module qualifier" in {
    namedValue("def f: R = mod.sub.Mod::func").asserting { nv =>
      nv.runtimeStructure shouldBe Some(QualRef("func", "mod.sub.Mod"))
    }
  }

  it should "curry qualified calls with arguments" in {
    namedValue("def f: R = Mod::func(a)").asserting { nv =>
      nv.runtimeStructure shouldBe Some(App(QualRef("func", "Mod"), Ref("a")))
    }
  }

  "lambda expressions" should "convert single-parameter lambda" in {
    namedValue("def f: R = (a: A) -> b").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("a", Ref("A", T), Ref("b")))
    }
  }

  it should "curry multi-parameter lambda" in {
    namedValue("def f: R = (a: A, b: B) -> c").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("a", Ref("A", T), Lambda("b", Ref("B", T), Ref("c"))))
    }
  }

  it should "handle lambda without parentheses" in {
    namedValue("def f: R = a: A -> b").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("a", Ref("A", T), Ref("b")))
    }
  }

  it should "handle nested lambdas" in {
    namedValue("def f: R = (a: A) -> (b: B) -> c").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Lambda("a", Ref("A", T), Lambda("b", Ref("B", T), Ref("c"))))
    }
  }

  "literals" should "convert integer literals" in {
    namedValue("def f: R = 42").asserting { nv =>
      nv.runtimeStructure shouldBe Some(IntLit("42"))
    }
  }

  it should "convert string literals without quotes" in {
    namedValue("def f: R = \"hello\"").asserting { nv =>
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
      nv.typeStack.signatureStructure shouldBe Ref("Type", T)
    }
  }

  // Note: Signature uses FunctionLiteral to preserve parameter names (A: Type -> Type)
  it should "generate type function with generic param and argument in typeStack" in {
    namedValue("data Box[A]", QualifiedName("Box", Qualifier.Type)).asserting { nv =>
      nv.typeStack.signatureStructure shouldBe Lambda("A", Ref("Type"), Ref("Type", T))
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
        App(
          App(Ref("Function", T), Ref("Name", T)),
          App(App(Ref("Function", T), Ref("Age", T)), Ref("Person", Qualifier.Type))
        )
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
      nvs.map(_.qualifiedName.value) should contain allOf (QualifiedName("name", Qualifier.Default), QualifiedName(
        "age",
        Qualifier.Default
      ))
    }
  }

  it should "generate accessor with argument in typeStack" in {
    namedValue("data Person(name: Name)", QualifiedName("name", Qualifier.Default)).asserting { nv =>
      nv.typeStack.signatureStructure shouldBe
        App(App(Ref("Function", T), Ref("Person", Qualifier.Type)), Ref("Name", T))
    }
  }

  it should "generate accessor with generic param and argument in typeStack" in {
    namedValue("data Box[A](value: A)", QualifiedName("value", Qualifier.Default)).asserting { nv =>
      // The accessor type is (A :: Type) -> Function(Box^Type(A), A)
      nv.typeStack.signatureStructure shouldBe Lambda(
        "A",
        Ref("Type"),
        App(App(Ref("Function", T), App(Ref("Box", Qualifier.Type), Ref("A", T))), Ref("A", T))
      )
    }
  }

  "union data definitions" should "generate constructor for each variant" in {
    namedValues("data Maybe = Nothing | Just(value: A)").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should contain allOf (
        QualifiedName("Maybe", Qualifier.Type),
        QualifiedName("Nothing", Qualifier.Default),
        QualifiedName("Just", Qualifier.Default)
      )
    }
  }

  it should "not generate accessors for multi-constructor data" in {
    namedValues("data Maybe = Nothing | Just(value: A)").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should not contain QualifiedName("value", Qualifier.Default)
    }
  }

  it should "generate fieldless constructor returning data type" in {
    namedValue("data Maybe = Nothing | Just(value: A)", QualifiedName("Nothing", Qualifier.Default)).asserting { nv =>
      nv.typeStack.signatureStructure shouldBe Ref("Maybe", Qualifier.Type)
    }
  }

  it should "generate constructor with fields returning data type" in {
    namedValue("data Maybe = Nothing | Just(value: A)", QualifiedName("Just", Qualifier.Default)).asserting { nv =>
      nv.typeStack.signatureStructure shouldBe
        App(App(Ref("Function", T), Ref("A", T)), Ref("Maybe", Qualifier.Type))
    }
  }

  it should "generate generic union constructors with shared type params" in {
    namedValue("data Maybe[A] = Nothing | Just(value: A)", QualifiedName("Just", Qualifier.Default)).asserting { nv =>
      nv.typeStack.signatureStructure shouldBe
        Lambda(
          "A",
          Ref("Type"),
          App(App(Ref("Function", T), Ref("A", T)), App(Ref("Maybe", Qualifier.Type), Ref("A", T)))
        )
    }
  }

  it should "generate generic fieldless constructor with type params" in {
    namedValue("data Maybe[A] = Nothing | Just(value: A)", QualifiedName("Nothing", Qualifier.Default)).asserting {
      nv =>
        nv.typeStack.signatureStructure shouldBe
          Lambda("A", Ref("Type"), App(Ref("Maybe", Qualifier.Type), Ref("A", T)))
    }
  }

  it should "generate accessors for single constructor with = syntax" in {
    namedValues("data Box[A] = Box(value: A)").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should contain(QualifiedName("value", Qualifier.Default))
    }
  }

  it should "generate three fieldless constructors" in {
    namedValues("data Color = Red | Green | Blue").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should contain allOf (
        QualifiedName("Color", Qualifier.Type),
        QualifiedName("Red", Qualifier.Default),
        QualifiedName("Green", Qualifier.Default),
        QualifiedName("Blue", Qualifier.Default)
      )
    }
  }

  "type-specific eliminator" should "be generated for union data" in {
    namedValues("data Maybe = Nothing | Just(value: A)").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should contain(QualifiedName("handleMaybeWith", Qualifier.Default))
    }
  }

  it should "be generated for single-constructor data with fields" in {
    namedValues("data Box[A](value: A)").asserting { nvs =>
      nvs.map(_.qualifiedName.value) should contain(QualifiedName("handleBoxWith", Qualifier.Default))
    }
  }

  it should "not be generated for abstract data" in {
    namedValues("data Abstract").asserting { nvs =>
      nvs.map(_.qualifiedName.value).filter(_.name.startsWith("handle")) shouldBe empty
    }
  }

  it should "have public visibility" in {
    namedValue("data Box[A](value: A)", QualifiedName("handleBoxWith", Qualifier.Default)).asserting { nv =>
      nv.visibility shouldBe Visibility.Public
    }
  }

  it should "have correct type for single-constructor data" in {
    namedValue("data Box[A](value: A)", QualifiedName("handleBoxWith", Qualifier.Default)).asserting { nv =>
      // handleBoxWith[A, R](obj: Box[A], boxCase: Function[A, R]): R
      nv.typeStack.signatureStructure shouldBe Lambda(
        "A",
        Ref("Type"),
        Lambda(
          "R",
          Ref("Type"),
          App(
            App(Ref("Function", T), App(Ref("Box", T), Ref("A", T))),
            App(App(Ref("Function", T), App(App(Ref("Function", T), Ref("A", T)), Ref("R", T))), Ref("R", T))
          )
        )
      )
    }
  }

  it should "have correct type for union data with fieldless constructor" in {
    namedValue("data Maybe[A] = Nothing | Just(value: A)", QualifiedName("handleMaybeWith", Qualifier.Default)).asserting {
      nv =>
        // handleMaybeWith[A, R](obj: Maybe[A], nothingCase: Function[Unit, R], justCase: Function[A, R]): R
        nv.typeStack.signatureStructure shouldBe Lambda(
          "A",
          Ref("Type"),
          Lambda(
            "R",
            Ref("Type"),
            App(
              App(Ref("Function", T), App(Ref("Maybe", T), Ref("A", T))),
              App(
                App(Ref("Function", T), App(App(Ref("Function", T), Ref("Unit", T)), Ref("R", T))),
                App(App(Ref("Function", T), App(App(Ref("Function", T), Ref("A", T)), Ref("R", T))), Ref("R", T))
              )
            )
          )
        )
    }
  }

  it should "have correct type for enum-like data" in {
    namedValue("data Color = Red | Green | Blue", QualifiedName("handleColorWith", Qualifier.Default)).asserting { nv =>
      // handleColorWith[R](obj: Color, redCase: Function[Unit, R], greenCase: Function[Unit, R], blueCase: Function[Unit, R]): R
      nv.typeStack.signatureStructure shouldBe Lambda(
        "R",
        Ref("Type"),
        App(
          App(Ref("Function", T), Ref("Color", T)),
          App(
            App(Ref("Function", T), App(App(Ref("Function", T), Ref("Unit", T)), Ref("R", T))),
            App(
              App(Ref("Function", T), App(App(Ref("Function", T), Ref("Unit", T)), Ref("R", T))),
              App(App(Ref("Function", T), App(App(Ref("Function", T), Ref("Unit", T)), Ref("R", T))), Ref("R", T))
            )
          )
        )
      )
    }
  }

  it should "avoid clashing result parameter name with existing generic parameter R" in {
    namedValue("data Foo[R](value: R)", QualifiedName("handleFooWith", Qualifier.Default)).asserting { nv =>
      // handleFooWith[R, R0](obj: Foo[R], fooCase: Function[R, R0]): R0
      nv.typeStack.signatureStructure shouldBe Lambda(
        "R",
        Ref("Type"),
        Lambda(
          "R0",
          Ref("Type"),
          App(
            App(Ref("Function", T), App(Ref("Foo", T), Ref("R", T))),
            App(App(Ref("Function", T), App(App(Ref("Function", T), Ref("R", T)), Ref("R0", T))), Ref("R0", T))
          )
        )
      )
    }
  }

  it should "have abstract body" in {
    namedValue("data Box[A](value: A)", QualifiedName("handleBoxWith", Qualifier.Default)).asserting { nv =>
      nv.runtime shouldBe None
    }
  }

  "flat expressions" should "pass through as FlatExpression in core" in {
    namedValue("def f: T = b + c").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Flat(Seq(Ref("b"), Ref("+"), Ref("c"))))
    }
  }

  it should "pass through multi-atom expression" in {
    namedValue("def f: T = g x y").asserting { nv =>
      nv.runtimeStructure shouldBe Some(Flat(Seq(Ref("g"), Ref("x"), Ref("y"))))
    }
  }

  "fixity" should "be Application for plain def" in {
    namedValue("def f: T = b").asserting(_.fixity shouldBe Fixity.Application)
  }

  it should "be Prefix for prefix def" in {
    namedValue("prefix def !(a: T): T = a", QualifiedName("!", Qualifier.Default)).asserting {
      _.fixity shouldBe Fixity.Prefix
    }
  }

  it should "be Infix(Left) for infix left def" in {
    namedValue("infix left def +(a: T, b: T): T = a", QualifiedName("+", Qualifier.Default)).asserting {
      _.fixity shouldBe Fixity.Infix(Fixity.Associativity.Left)
    }
  }

  it should "be Infix(Left) for infix def with default associativity" in {
    namedValue("infix def or(a: T, b: T): T = a", QualifiedName("or", Qualifier.Default)).asserting {
      _.fixity shouldBe Fixity.Infix(Fixity.Associativity.Left)
    }
  }

  it should "be Postfix for postfix def" in {
    namedValue("postfix def ++(a: T): T = a", QualifiedName("++", Qualifier.Default)).asserting {
      _.fixity shouldBe Fixity.Postfix
    }
  }

  "complex scenarios" should "handle mixed functions and data" in {
    runEngineForCoreAST("data A\ndef f: A").asserting { ast =>
      ast.namedValues.map(_.qualifiedName.value) should contain allOf (QualifiedName(
        "A",
        Qualifier.Type
      ), QualifiedName("f", Qualifier.Default))
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
              App(Ref("Function", T), Ref("A", T)),
              App(App(Ref("Function", T), Ref("B", T)), App(App(Ref("Pair", Qualifier.Type), Ref("A", T)), Ref("B", T)))
            )
          )
        )
    }
  }

  it should "preserve import statements in core AST" in {
    runEngineForCoreAST("import a.b.C\ndef f: R").asserting { ast =>
      ast.importStatements.map(i => (i.packageNames.map(_.value) :+ i.moduleName.value).mkString(".")) shouldBe
        Seq("a.b.C")
    }
  }

  // Structural representation of expressions (ignores source positions)
  sealed trait ExprStructure
  case class Ref(name: String, qualifier: Qualifier = Qualifier.Default)          extends ExprStructure
  case class QualRef(name: String, module: String)                                extends ExprStructure
  case class App(target: ExprStructure, arg: ExprStructure)                       extends ExprStructure
  case class Lambda(param: String, paramType: ExprStructure, body: ExprStructure) extends ExprStructure
  case class IntLit(value: String)                                                extends ExprStructure
  case class StrLit(value: String)                                                extends ExprStructure
  case class Flat(parts: Seq[ExprStructure])                                      extends ExprStructure
  case object Empty                                                               extends ExprStructure

  extension (nv: NamedValue) {
    def runtimeStructure: Option[ExprStructure] = nv.runtime.map(_.value.structure)
  }

  extension (stack: TypeStack[Expression]) {
    def signatureStructure: ExprStructure = stack.signature.structure
    // Gets the first expression in the stack (signature for TypeStack)
    def firstStructure: ExprStructure     = stack.levels.head.structure
  }

  extension (expr: Expression) {
    def structure: ExprStructure = expr match {
      case NamedValueReference(name, None, _)       => Ref(name.value.name, name.value.qualifier)
      case NamedValueReference(name, Some(qual), _) => QualRef(name.value.name, qual.value)
      case FunctionApplication(target, arg)         => App(target.value.firstStructure, arg.value.firstStructure)
      case FunctionLiteral(param, paramType, body)  =>
        Lambda(param.value, paramType.map(_.firstStructure).getOrElse(Empty), body.value.firstStructure)
      case IntegerLiteral(lit)                      => IntLit(lit.value)
      case StringLiteral(lit)                       => StrLit(lit.value)
      case FlatExpression(parts)                    => Flat(parts.map(_.value.signature.structure))
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
