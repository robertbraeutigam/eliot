package com.vanillasource.eliot.eliotc.abilitycheck

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
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
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.symbolic.processor.SymbolicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer

class AbilityCheckProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq(ModuleName.systemFunctionModuleName)),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      OperatorResolverProcessor(),
      SymbolicTypeCheckProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      AbilityCheckProcessor()
    ) {
  "ability calls" should "type check when calling ability with concrete type" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "succeed when calling ability with generic type parameter covered by constraint" in {
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
        "Cannot prove ability 'Show' is available for given type. (Ability implementations require concrete types for now)." at "show"
      )
    )
  }

  it should "fail when no implementation exists for the concrete type" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("The type parameter 'Int' does not implement ability 'Show'." at "show"))
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
      AbilityCheckedValue.Key(ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default))),
      systemImports
    ).map(result => toTestErrors(result._1))
}
