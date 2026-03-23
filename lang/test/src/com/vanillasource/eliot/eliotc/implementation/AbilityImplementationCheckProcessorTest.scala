package com.vanillasource.eliot.eliotc.implementation

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.eval.processor.{DataTypeEvaluator, ExistingNamedValueEvaluator, SystemValueEvaluator}
import com.vanillasource.eliot.eliotc.implementation.processor.{
  AbilityImplementationCheckProcessor,
  AbilityImplementationProcessor
}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.{
  ModuleNamesProcessor,
  ModuleValueProcessor,
  UnifiedModuleNamesProcessor,
  UnifiedModuleValueProcessor
}
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.monomorphize.processor.MonomorphicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer

class AbilityImplementationCheckProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      SystemValueEvaluator(),
      ExistingNamedValueEvaluator(),
      DataTypeEvaluator(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq(ModuleName.systemFunctionModuleName)),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor(),
      OperatorResolverProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      MonomorphicTypeCheckProcessor()
    ) {
  "ability implementation check" should "pass when all methods are provided with correct signatures" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail when a required ability method is missing" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A\ndef display(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("Ability implementation is missing method 'display'." at "Show"))
  }

  it should "fail when an extra method not in the ability is defined" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x\ndef extra(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("Method not defined in ability." at "extra"))
  }

  it should "fail when an implementation method has the wrong signature" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndata Bool\nimplement Show[Int] { def show(x: Bool): Bool = x }\ndef f(x: Int): Int = show(x)"
    ).asserting { errors =>
      errors.length shouldBe 1
      errors.head.message should include("Signature of implementation does not match the ability definition")
      errors.head.highlight shouldBe "show"
    }
  }

  it should "pass when a non-abstract ability method is not present in the implementation" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A\ndef extra(x: A): A = x }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "resolve default ability implementation when called" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A\ndef extra(x: A): A = x }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = extra(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "resolve default ability implementation that calls another ability method" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A\ndef display(x: A): A = show(x) }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = display(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail when no implementations are provided at all for an ability with all-default methods" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A = x }\ndata Int\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("The type parameter 'Int' does not implement ability 'Show'." at "Show"))
  }

  it should "resolve default ability implementation that calls another default ability implementation" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A = x\ndef display(x: A): A = show(x) }\ndata Int\nimplement Show[Int]\ndef f(a: Int): Int = display(a)"
    ).asserting(_ shouldBe Seq.empty)
  }

  // --- Ability call resolution tests (migrated from AbilityCheckProcessorTest) ---

  "ability calls" should "succeed when calling ability with generic type parameter covered by constraint" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndef f[A ~ Show[A]](x: A): A = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "succeed when calling ability with default generic type parameter covered by constraint" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndef f[A ~ Show](x: A): A = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "support empty abilities to be declared" in {
    runEngineForErrors(
      "ability Marker[A]\ndef f[A ~ Marker](x: A): A"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "support empty ability implementations" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail when calling ability with abstract type parameter not covered by constraint" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndef f[A](x: A): A = show(x)"
    ).asserting(
      _ shouldBe Seq(
        "Cannot prove ability 'Show' is available for given type." at "show"
      )
    )
  }

  it should "fail when no implementation exists for the concrete type" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("The type parameter 'Int' does not implement ability 'Show'." at "Show"))
  }

  it should "check derived abilities" in {
    runEngineForErrors("""
        data String
        def someString: String

        ability Show[A] {
          def show(a: A): String
        }

        implement Show[String] {
          def show(str: String): String = str
        }

        data Box[A](content: A)

        implement[A ~ Show] Show[Box[A]] {
          def show(box: Box[A]): String = show(content(box))
        }

        def f: String = show(Box(someString))
    """).asserting(_ shouldBe Seq.empty)
  }

  private def runEngineForErrors(source: String): IO[Seq[TestError]] =
    runGenerator(
      source,
      MonomorphicValue.Key(ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default)), Seq.empty),
      systemImports
    ).map(result => toTestErrors(result._1))
}
