package com.vanillasource.eliot.eliotc.uncurry

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.datafunctions.DataFunctionsProcessor
import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.systemFunctionModuleName
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference
import com.vanillasource.eliot.eliotc.resolve.processor.FunctionResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.typesystem.processor.TypeCheckProcessor
import com.vanillasource.eliot.eliotc.uncurry.{UncurriedFunction, UncurriedTypedExpression, UncurryingProcessor}

class UncurryingProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      DataFunctionsProcessor(),
      ModuleDataProcessor(Seq(systemFunctionModuleName)),
      ModuleFunctionProcessor(Seq(systemFunctionModuleName)),
      ModuleNamesProcessor(),
      UnifiedModuleFunctionProcessor(),
      UnifiedModuleDataProcessor(),
      UnifiedModuleNamesProcessor(),
      FunctionResolver(),
      TypeCheckProcessor(),
      UncurryingProcessor()
    ) {

  "uncurrying processor" should "convert single-parameter function to uncurried form" in {
    runEngineForUncurriedFunction("data A\nf(a: A): A = a")
      .asserting { func =>
        func.definition.parameters.length shouldBe 1
        func.definition.parameters.head.name.value shouldBe "a"
      }
  }

  it should "convert multi-parameter curried function to uncurried form" in {
    runEngineForUncurriedFunction("data A\ndata B\nf(a: A, b: B): A = a")
      .asserting { func =>
        func.definition.parameters.length shouldBe 2
        func.definition.parameters(0).name.value shouldBe "a"
        func.definition.parameters(1).name.value shouldBe "b"
      }
  }

  it should "convert three-parameter curried function to uncurried form" in {
    runEngineForUncurriedFunction("data A\ndata B\ndata C\nf(a: A, b: B, c: C): A = a")
      .asserting { func =>
        func.definition.parameters.length shouldBe 3
        func.definition.parameters(0).name.value shouldBe "a"
        func.definition.parameters(1).name.value shouldBe "b"
        func.definition.parameters(2).name.value shouldBe "c"
      }
  }

  it should "uncurry nested function applications" in {
    runEngineForUncurriedFunction("data A\ng(a: A, b: A): A\nf(x: A): A = g(x, x)")
      .asserting { func =>
        val body = func.definition.body.get
        body.value.expression match {
          case UncurriedTypedExpression.FunctionApplication(target, arguments) =>
            arguments.length shouldBe 2
            // Both arguments should be parameter references to 'x'
            all(arguments.map(_.value.expression)) shouldBe a[UncurriedTypedExpression.ParameterReference]
          case other                                                           =>
            fail(s"Expected FunctionApplication but got: $other")
        }
      }
  }

  it should "preserve correct return type from curried function type" in {
    runEngineForUncurriedFunction("data A\ndata B\ndata C\nf(a: A, b: B): C = g\ng: C")
      .asserting { func =>
        func.definition.parameters.length shouldBe 2
        // The return type should be C, not Function[B, C]
        TypeReference.unqualified.show(func.definition.returnType) shouldBe "C"
      }
  }

  it should "handle zero-parameter functions" in {
    runEngineForUncurriedFunction("data A\nf: A = g\ng: A")
      .asserting { func =>
        func.definition.parameters.length shouldBe 0
      }
  }

  it should "preserve value references in function body" in {
    runEngineForUncurriedFunction("data A\nf: A = g\ng: A")
      .asserting { func =>
        func.definition.body.get.value.expression shouldBe a[UncurriedTypedExpression.ValueReference]
      }
  }

  it should "not uncurry a Function return type that was declared by the user" in {
    runEngineForUncurriedFunction("data A\ndata B\ndata C\nf(a: A): Function[B, C] = g\ng: Function[B, C]")
      .asserting { func =>
        (func.definition.parameters.map(_.name.value), TypeReference.unqualified.show(func.definition.returnType)) shouldBe
          (Seq("a"), "Function[B,C]")
      }
  }

  "functions without body" should "uncurry signature with no parameters" in {
    runEngineForUncurriedFunction("data A\nf: A")
      .asserting { func =>
        func.definition.parameters.length shouldBe 0
        TypeReference.unqualified.show(func.definition.returnType) shouldBe "A"
        func.definition.body shouldBe None
      }
  }

  it should "uncurry signature with one parameter" in {
    runEngineForUncurriedFunction("data A\ndata B\nf(a: A): B")
      .asserting { func =>
        func.definition.parameters.length shouldBe 1
        TypeReference.unqualified.show(func.definition.returnType) shouldBe "B"
        func.definition.body shouldBe None
      }
  }

  it should "uncurry signature with multiple parameters" in {
    runEngineForUncurriedFunction("data A\ndata B\ndata C\nf(a: A, b: B): C")
      .asserting { func =>
        func.definition.parameters.length shouldBe 2
        TypeReference.unqualified.show(func.definition.returnType) shouldBe "C"
        func.definition.body shouldBe None
      }
  }

  it should "uncurry signature with three parameters" in {
    runEngineForUncurriedFunction("data A\ndata B\ndata C\ndata D\nf(a: A, b: B, c: C): D")
      .asserting { func =>
        func.definition.parameters.length shouldBe 3
        TypeReference.unqualified.show(func.definition.returnType) shouldBe "D"
        func.definition.body shouldBe None
      }
  }

  it should "uncurry signature with generic parameters" in {
    runEngineForUncurriedFunction("f[A](a: A): A")
      .asserting { func =>
        func.definition.genericParameters.length shouldBe 1
        func.definition.parameters.length shouldBe 1
        TypeReference.unqualified.show(func.definition.returnType) shouldBe "A"
        func.definition.body shouldBe None
      }
  }

  it should "extract correct parameter types from curried function type" in {
    runEngineForUncurriedFunction("data A\ndata B\ndata C\nf(a: A, b: B): C")
      .asserting { func =>
        func.definition.parameters.map(p => TypeReference.unqualified.show(p.typeReference)) shouldBe Seq("A", "B")
      }
  }

  private def runEngineForUncurriedFunction(source: String): IO[UncurriedFunction] =
    runGenerator(source, UncurriedFunction.Key(FunctionFQN(testModuleName, "f")), systemImports)
      .flatMap { case (errors, facts) =>
        if (errors.nonEmpty) {
          IO.raiseError(new Exception(s"Compilation errors: ${errors.map(_.message).mkString(", ")}"))
        } else {
          facts.values.collectFirst { case f: UncurriedFunction if f.ffqn.functionName == "f" => f } match {
            case Some(func) => IO.pure(func)
            case None       => IO.raiseError(new Exception("No uncurried function 'f' found in results"))
          }
        }
      }
}
