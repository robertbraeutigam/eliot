package com.vanillasource.eliot.eliotc.implementation

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.abilitycheck.{AbilityCheckProcessor, AbilityCheckedValue}
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.implementation.processor.{AbilityImplementationCheckProcessor, AbilityImplementationProcessor}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.{ModuleNamesProcessor, ModuleValueProcessor, UnifiedModuleNamesProcessor, UnifiedModuleValueProcessor}
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.symbolic.processor.SymbolicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer

class AbilityImplementationCheckProcessorTest
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
  "ability implementation check" should "pass when all methods are provided with correct signatures" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail when a required ability method is missing" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A\ndef display(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ should contain("display^Ability(Show): Ability implementation is missing method 'display'."))
  }

  it should "fail when an extra method not in the ability is defined" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x\ndef extra(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_.exists(e => e.contains("extra") && e.contains("not defined in the ability")) shouldBe true)
  }

  it should "fail when an implementation method has the wrong signature" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndata Bool\nimplement Show[Int] { def show(x: Bool): Bool = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_.exists(e => e.contains("show") && e.contains("does not match the ability definition")) shouldBe true)
  }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(
      source,
      AbilityCheckedValue.Key(ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default))),
      systemImports
    ).map(_._1.map(_.message))
}
