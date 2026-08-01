package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{ParameterReference, ValueReference}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.resolve.fact.{QualifiedName as ResolveQualifiedName, Qualifier as ResolveQualifier}

/** The native meta-declaration check ([[NativeMetaDeclarationProcessor]]): a native (body-less) value whose ground
  * return type carries refinement meta must declare a `^Meta` transfer companion, else its check aborts (blocking
  * codegen). A bodied value, a plain-generic-payload return, a slotless return, and a value with the companion all pass.
  */
class NativeMetaDeclarationProcessorTest extends ProcessorTest(NativeMetaDeclarationProcessor()) {

  private val intModule = ModuleName(Seq("eliot", "lang"), "Int")
  private val intFqn    = ValueFQN(intModule, QualifiedName("Int", Qualifier.Type))
  private val intType   = GroundValue.Structure(intFqn, Seq.empty, GroundValue.Type)
  private val natFqn    = ValueFQN(testModuleName, default("conjure"))

  /** `Int` declares an `Int$Meta` structure in its module (compiler pool), so an `Int` return carries non-`Unit` meta. */
  private val intIsMetaCarrying =
    UnifiedModuleNames(intModule, Map(QualifiedName("Int$Meta", Qualifier.Type) -> Visibility.Public), Platform.Compiler)

  private def nativeMono(returnType: GroundValue): MonomorphicValue =
    MonomorphicValue(natFqn, Seq.empty, sourced(default("conjure")), returnType, None, Set.empty)

  private def bodiedMono(returnType: GroundValue): MonomorphicValue =
    MonomorphicValue(
      natFqn,
      Seq.empty,
      sourced(default("conjure")),
      returnType,
      Some(sourced(MonomorphicExpression.IntegerLiteral(sourced(BigInt(0))))),
      Set.empty
    )

  /** The native's own module names in the compiler pool, optionally carrying its `conjure^Meta` companion. */
  private def natModuleNames(withCompanion: Boolean): UnifiedModuleNames =
    UnifiedModuleNames(
      testModuleName,
      if (withCompanion) Map(QualifiedName("conjure", Qualifier.Meta) -> Visibility.Public) else Map.empty,
      Platform.Compiler
    )

  private def resolvedValue(signature: OperatorResolvedExpression): OperatorResolvedValue =
    OperatorResolvedValue(
      natFqn,
      sourced(ResolveQualifiedName("conjure", ResolveQualifier.Default)),
      None,
      sourced(signature),
      platform = Platform.Runtime
    )

  private def check(facts: Seq[CompilerFact]) =
    runGeneratorWithFacts(facts, NativeMetaDeclared.Key(natFqn, Seq.empty))

  "NativeMetaDeclarationProcessor" should "reject a native producing a meta-carrying type without a declared meta" in {
    check(Seq(nativeMono(intType), intIsMetaCarrying, natModuleNames(withCompanion = false)))
      .asserting { case (fact, errors) =>
        (fact, errors.map(_.message)) shouldBe (None, Seq(NativeMetaDeclarationProcessorTest.expectedMessage))
      }
  }

  it should "accept the same native once it declares its meta companion" in {
    check(Seq(nativeMono(intType), intIsMetaCarrying, natModuleNames(withCompanion = true)))
      .asserting { case (fact, errors) => (fact.isDefined, errors) shouldBe (true, Seq.empty) }
  }

  it should "accept a native whose return is a plain generic payload, companion absent" in {
    // `def conjure[A]: A` instantiated at `A := Int`: the ground return is `Int`, but the abstract return is a bound
    // binder, so the native forwards a payload rather than producing the range — exempt (effects-as-rows rule 4).
    check(
      Seq(
        nativeMono(intType),
        intIsMetaCarrying,
        natModuleNames(withCompanion = false),
        resolvedValue(ParameterReference(sourced("A")))
      )
    ).asserting { case (fact, errors) => (fact.isDefined, errors) shouldBe (true, Seq.empty) }
  }

  it should "reject even with a concrete abstract return but no companion" in {
    // `def conjure: Int` — the abstract return is the concrete `Int` reference, a genuine producer.
    check(
      Seq(
        nativeMono(intType),
        intIsMetaCarrying,
        natModuleNames(withCompanion = false),
        resolvedValue(ValueReference(sourced(intFqn)))
      )
    ).asserting { case (fact, errors) =>
      (fact, errors.map(_.message)) shouldBe (None, Seq(NativeMetaDeclarationProcessorTest.expectedMessage))
    }
  }

  it should "accept a bodied value (its imprecise ⊤ is not a silent native gap)" in {
    check(Seq(bodiedMono(intType), intIsMetaCarrying, natModuleNames(withCompanion = false)))
      .asserting { case (fact, errors) => (fact.isDefined, errors) shouldBe (true, Seq.empty) }
  }

  it should "accept a native whose return type carries no meta" in {
    val stringFqn  = ValueFQN(ModuleName(Seq("eliot", "lang"), "String"), QualifiedName("String", Qualifier.Type))
    val stringType = GroundValue.Structure(stringFqn, Seq.empty, GroundValue.Type)
    check(Seq(nativeMono(stringType), natModuleNames(withCompanion = false)))
      .asserting { case (fact, errors) => (fact.isDefined, errors) shouldBe (true, Seq.empty) }
  }
}

object NativeMetaDeclarationProcessorTest {
  val expectedMessage: String =
    "The native 'conjure' produces a value of a type that carries refinement meta-information but does not declare " +
      "what range it produces; add a return brace stating its meta (e.g. `: Int {Interval(0, 255)}`, or " +
      "`: Int {unbounded}` when the range is the whole domain)."
}
