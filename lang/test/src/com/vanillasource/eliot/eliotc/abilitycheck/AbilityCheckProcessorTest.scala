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

  it should "fail when calling ability with abstract type parameter" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndef f[A](x: A): A = show(x)"
    ).asserting(
      _ shouldBe Seq(
        "Cannot call ability 'Show' with abstract type parameter. Ability implementations require concrete types."
      )
    )
  }

  it should "fail when no implementation exists for the concrete type" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("The type parameter 'Int' does not implement ability 'Show'."))
  }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(
      source,
      AbilityCheckedValue.Key(ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default))),
      systemImports
    ).map(_._1.map(_.message))
}
