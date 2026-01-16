package com.vanillasource.eliot.eliotc.typesystem2

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName => ModuleName2, ValueFQN}
import com.vanillasource.eliot.eliotc.module2.processor.*
import com.vanillasource.eliot.eliotc.resolve2.processor.ValueResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.typesystem2.fact.TypeCheckedValue
import com.vanillasource.eliot.eliotc.typesystem2.processor.SymbolicTypeCheckProcessor

class SymbolicTypeCheckProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq(ModuleName2.systemFunctionModuleName)),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      SymbolicTypeCheckProcessor()
    ) {
  private val testModuleName2 = ModuleName2(Seq.empty, "Test")

  "function call" should "compile if same number of arguments" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\nf: TypeA = g\ng: TypeA")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not compile if call site has arguments, but definition doesn't" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\nf: TypeA = g(1)\ng: TypeA")
      .asserting(
        _ shouldBe Seq("Target of function application is not a Function. Possibly too many arguments.")
      )
  }

  "processor" should "produce type checked results if arities are ok" in {
    runEngineForTypedValues("data TypeA(fieldA: TypeA)\nf: TypeA = g\ng: TypeA")
      .asserting(_.length shouldBe 1)
  }

  it should "not produce type checked results if arities mismatch" in {
    runEngineForTypedValues("data TypeA(fieldA: TypeA)\nf: TypeA = g(3)\ng: TypeA")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail only once when a function is used wrong" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\ndata TypeB(fieldB: TypeB)\nmyA: TypeA\nf: TypeB = myA")
      .asserting(_ shouldBe Seq("Type mismatch."))
  }

  it should "fail if parameter is of wrong type" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\ndata TypeB(fieldB: TypeB)\nf(p: TypeB): TypeA = p")
      .asserting(_ shouldBe Seq("Lambda body type mismatch."))
  }

  "functions without body" should "be type checked successfully with simple return type" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\nf: TypeA")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be type checked successfully with one parameter" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\ndata TypeB(fieldB: TypeB)\nf(x: TypeA): TypeB")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be type checked successfully with multiple parameters" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\ndata TypeB(fieldB: TypeB)\ndata TypeC(fieldC: TypeC)\nf(x: TypeA, y: TypeB): TypeC")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "produce type checked value fact for function without body" in {
    runEngineForTypedValue("data TypeA(fieldA: TypeA)\nf: TypeA")
      .asserting { value =>
        value.vfqn.name shouldBe "f"
        value.definition.body shouldBe None
      }
  }

  "parameter usage" should "type check when parameter type matches return type" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\nf(x: TypeA): TypeA = x")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail when parameter type does not match return type" in {
    runEngineForErrors("data TypeA(fieldA: TypeA)\ndata TypeB(fieldB: TypeB)\nf(x: TypeA): TypeB = x")
      .asserting(_ shouldBe Seq("Lambda body type mismatch."))
  }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(source, TypeCheckedValue.Key(ValueFQN(testModuleName2, "f")), systemImports)
      .map(_._1.map(_.message))

  private def runEngineForTypedValues(source: String): IO[Seq[ValueFQN]] =
    runGenerator(source, TypeCheckedValue.Key(ValueFQN(testModuleName2, "f")), systemImports).map { case (_, facts) =>
      facts.values.collect { case TypeCheckedValue(vfqn, _) =>
        vfqn
      }.toSeq
    }

  private def runEngineForTypedValue(source: String): IO[TypeCheckedValue] =
    runGenerator(source, TypeCheckedValue.Key(ValueFQN(testModuleName2, "f")), systemImports)
      .flatMap { case (errors, facts) =>
        if (errors.nonEmpty) {
          IO.raiseError(new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}"))
        } else {
          facts.values.collectFirst { case v: TypeCheckedValue if v.vfqn.name == "f" => v } match {
            case Some(value) => IO.pure(value)
            case None        => IO.raiseError(new Exception("No type checked value 'f' found in results"))
          }
        }
      }
}
