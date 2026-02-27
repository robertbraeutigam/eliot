package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.abilitycheck.{AbilityCheckProcessor, AbilityCheckedValue}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types, Value}
import com.vanillasource.eliot.eliotc.eval.processor.{DataTypeEvaluator, ExistingNamedValueEvaluator, SystemValueEvaluator}
import com.vanillasource.eliot.eliotc.implementation.processor.{AbilityImplementationCheckProcessor, AbilityImplementationProcessor}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.{MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.symbolic.processor.SymbolicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer

class MonomorphicTypeCheckProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      SystemValueEvaluator(),
      ExistingNamedValueEvaluator(),
      DataTypeEvaluator(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(
        Seq(
          ModuleName.systemFunctionModuleName,
          ModuleName(ModuleName.defaultSystemPackage, "Number"),
          ModuleName(ModuleName.defaultSystemPackage, "String"),
          ModuleName(ModuleName.defaultSystemPackage, "BigInteger")
        )
      ),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor(),
      OperatorResolverProcessor(),
      SymbolicTypeCheckProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      AbilityCheckProcessor(),
      MonomorphicTypeCheckProcessor()
    ) {

  override val systemImports: Seq[SystemImport] = Seq(
    SystemImport("Function", "data Function[A, B]"),
    SystemImport("Number", "data Int"),
    SystemImport("String", "data String"),
    SystemImport("BigInteger", "data BigInteger")
  )

  private val intType: Value =
    Types.dataType(ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Number"), QualifiedName("Int", Qualifier.Type)))

  private val stringType: Value =
    Types.dataType(
      ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type))
    )

  "MonomorphicTypeCheckProcessor" should "monomorphize non-generic value" in {
    runEngineForMonomorphicValue("def f: Int")
      .asserting { result =>
        result.typeArguments shouldBe Seq.empty
        showType(result.signature) shouldBe "Int"
        result.runtime shouldBe None
      }
  }

  it should "monomorphize identity function with Int" in {
    runEngineForMonomorphicValue("def id[A](a: A): A = a", "id", Seq(intType))
      .asserting { result =>
        showType(result.signature) shouldBe "Function[Int, Int]"
        result.runtime.get.value shouldBe a[MonomorphicExpression.FunctionLiteral]
      }
  }

  it should "monomorphize identity function with String" in {
    runEngineForMonomorphicValue("def id[A](a: A): A = a", "id", Seq(stringType))
      .asserting(result => showType(result.signature) shouldBe "Function[String, String]")
  }

  it should "monomorphize value with phantom type parameter" in {
    runEngineForMonomorphicValue("def f[I: BigInteger]: String")
      .asserting { result =>
        result.typeArguments shouldBe Seq.empty
        showType(result.signature) shouldBe "String"
      }
  }

  it should "monomorphize function with multiple type parameters" in {
    runEngineForMonomorphicValue(
      "def f[A, B](a: A, b: B): A = a",
      "f",
      Seq(intType, stringType)
    ).asserting(result => showType(result.signature) shouldBe "Function[Int, Function[String, Int]]")
  }

  // This tests an internal invariant unreachable from Eliot source code.
  // The symbolic type checker catches extra type arguments before monomorphization.
  it should "fail on type argument count mismatch" in {
    val idVfqn      = ValueFQN(testModuleName, default("id"))
    val dummyType   = intType
    val signature   = ExpressionValue.FunctionLiteral(
      "A",
      Value.Type,
      ExpressionValue.functionType(
        ExpressionValue.ParameterReference("A", Value.Type),
        ExpressionValue.ParameterReference("A", Value.Type)
      )
    )
    val typeChecked = AbilityCheckedValue(
      idVfqn,
      sourced(toSymbolic(default("id"))),
      signature,
      None
    )
    runGeneratorWithFacts(Seq(typeChecked), MonomorphicValue.Key(idVfqn, Seq(dummyType, dummyType)))
      .asserting { case (_, errors) =>
        errors.map(_.message) shouldBe Seq("Type argument count mismatch: expected 1, got 2")
      }
  }

  it should "monomorphize function literal in body" in {
    runEngineForMonomorphicValue("def f: Function[Int, Int] = (x: Int) -> x")
      .asserting(_.runtime.get.value shouldBe a[MonomorphicExpression.FunctionLiteral])
  }

  it should "monomorphize integer literal in body" in {
    runEngineForMonomorphicValue("def f: Int = 42")
      .asserting(_.runtime.get.value shouldBe a[MonomorphicExpression.IntegerLiteral])
  }

  it should "monomorphize string literal in body" in {
    runEngineForMonomorphicValue("def f: String = \"hello\"")
      .asserting(_.runtime.get.value shouldBe a[MonomorphicExpression.StringLiteral])
  }

  it should "monomorphize value reference to non-generic value" in {
    runEngineForMonomorphicValue("def constVal: Int\ndef f: Int = constVal")
      .asserting(_.runtime.get.value shouldBe a[MonomorphicExpression.MonomorphicValueReference])
  }

  it should "monomorphize function application" in {
    runEngineForMonomorphicValue("def id[A](a: A): A = a\ndef f: Int = id(42)")
      .asserting(_.runtime.get.value shouldBe a[MonomorphicExpression.FunctionApplication])
  }

  it should "handle direct recursion without infinite loop" in {
    import scala.concurrent.duration.*
    runEngineForMonomorphicValue("def f: Function[Int, Int] = f")
      .timeout(1.seconds)
      .asserting(_.runtime shouldBe defined)
  }

  it should "handle mutual recursion without infinite loop" in {
    import scala.concurrent.duration.*
    runEngineForMonomorphicValue("def f: Function[Int, Int] = g\ndef g: Function[Int, Int] = f")
      .timeout(1.seconds)
      .asserting(_.runtime shouldBe defined)
  }

  it should "resolve ability ref to concrete implementation when monomorphizing with concrete type" in {
    val source =
      """ability Show[A] {
        |  def show(x: A): A
        |}
        |implement Show[Int] {
        |  def show(x: Int): Int = x
        |}
        |def f[A ~ Show](x: A): A = show(x)""".stripMargin
    runEngineForMonomorphicValue(source, "f", Seq(intType))
      .asserting { result =>
        unwrapFunctionLiterals(result.runtime.get.value) match {
          case MonomorphicExpression.FunctionApplication(target, _) =>
            target.value.expression match {
              case MonomorphicExpression.MonomorphicValueReference(name, typeArgs) =>
                name.value.name.qualifier shouldBe a[Qualifier.AbilityImplementation]
                typeArgs shouldBe Seq.empty
              case other                                                           =>
                fail(s"Expected MonomorphicValueReference, got $other")
            }
          case other                                                 =>
            fail(s"Expected FunctionApplication, got $other")
        }
      }
  }

  private def triggerKey(name: String, typeArgs: Seq[Value]): MonomorphicValue.Key =
    MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs)

  private def runEngineForMonomorphicValue(
      source: String,
      name: String = "f",
      typeArgs: Seq[Value] = Seq.empty
  ): IO[MonomorphicValue] =
    runGenerator(source, triggerKey(name, typeArgs), systemImports).flatMap { case (errors, facts) =>
      if (errors.nonEmpty)
        IO.raiseError(new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}"))
      else
        facts.values.collectFirst { case v: MonomorphicValue if v.vfqn.name.name == name => v } match {
          case Some(v) => IO.pure(v)
          case None    => IO.raiseError(new Exception(s"No MonomorphicValue found for '$name'"))
        }
    }

  private def unwrapFunctionLiterals(expr: MonomorphicExpression.Expression): MonomorphicExpression.Expression =
    expr match {
      case MonomorphicExpression.FunctionLiteral(_, _, body) => unwrapFunctionLiterals(body.value.expression)
      case other                                              => other
    }

  private def showType(value: Value): String = value match {
    case Value.Structure(fields, Value.Type) =>
      val typeName = fields("$typeName").asInstanceOf[Value.Direct].value.asInstanceOf[ValueFQN].name.name
      val typeArgs = fields.removed("$typeName")
      if (typeArgs.isEmpty) typeName
      else s"$typeName[${typeArgs.toSeq.sortBy(_._1).map(f => showType(f._2)).mkString(", ")}]"
    case Value.Type                          => "Type"
    case _                                   => value.toString
  }
}
