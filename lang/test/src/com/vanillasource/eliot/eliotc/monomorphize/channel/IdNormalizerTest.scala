package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** The effects-as-channel **Id-normalization** rewrites ([[IdNormalizer]], docs/effects-as-channel.md §6): the identity
  * carrier's machinery — `runId`, the `Id` constructor, and the `Effect[Id]` combinators — is erased from a monomorphic
  * body. These are pure tree rewrites, exercised here directly over hand-built [[MonomorphicExpression]] nodes.
  */
class IdNormalizerTest extends AnyFlatSpec with Matchers {

  private val uri = URI.create("Test.els")

  private def at[A](a: A): Sourced[A] = Sourced(uri, PositionRange.zero, a)

  private val stringType: GroundValue = GroundValue.Structure(WellKnownTypes.stringFQN, Seq.empty, GroundValue.Type)
  private def idType(payload: GroundValue): GroundValue =
    GroundValue.Structure(WellKnownTypes.idFQN, Seq(payload), GroundValue.Type)
  private def fnType(from: GroundValue, to: GroundValue): GroundValue =
    GroundValue.Structure(WellKnownTypes.functionDataTypeFQN, Seq(from, to), GroundValue.Type)

  private def node(t: GroundValue, e: MonomorphicExpression.Expression): Sourced[MonomorphicExpression] =
    at(MonomorphicExpression(t, e))
  private def ref(fqn: ValueFQN, t: GroundValue): Sourced[MonomorphicExpression] =
    node(t, MonomorphicExpression.MonomorphicValueReference(at(fqn), Seq.empty))
  private def app(target: Sourced[MonomorphicExpression], arg: Sourced[MonomorphicExpression], t: GroundValue) =
    node(t, MonomorphicExpression.FunctionApplication(target, arg))
  private val str: Sourced[MonomorphicExpression] = node(stringType, MonomorphicExpression.StringLiteral(at("x")))

  private def effectId(method: String): ValueFQN =
    ValueFQN(WellKnownTypes.idModuleName, QualifiedName(method, Qualifier.AbilityImplementation("Effect", "Id")))

  private def normalizedExpr(top: MonomorphicExpression.Expression): MonomorphicExpression.Expression =
    IdNormalizer.normalize(at(top)).value

  "the Id-normalizer" should "erase a runId application to its argument" in {
    normalizedExpr(app(ref(WellKnownTypes.runIdFQN, fnType(idType(stringType), stringType)), str, stringType).value.expression)
      .shouldBe(str.value.expression)
  }

  it should "erase an Id constructor application to its argument" in {
    normalizedExpr(app(ref(WellKnownTypes.idConstructorFQN, fnType(stringType, idType(stringType))), str, idType(stringType)).value.expression)
      .shouldBe(str.value.expression)
  }

  it should "erase a pure@Effect[Id] application to its argument" in {
    normalizedExpr(app(ref(effectId("pure"), fnType(stringType, idType(stringType))), str, idType(stringType)).value.expression)
      .shouldBe(str.value.expression)
  }

  it should "collapse flatMap@Effect[Id](f, m) to the application f(m)" in {
    val f = ref(effectId("id"), fnType(stringType, idType(stringType))) // stand-in continuation function value
    val m = str
    val inner = app(ref(effectId("flatMap"), fnType(stringType, idType(stringType))), f, GroundValue.Type)
    normalizedExpr(app(inner, m, idType(stringType)).value.expression)
      .shouldBe(MonomorphicExpression.FunctionApplication(f, m))
  }

  it should "collapse map@Effect[Id](f, m) to the application f(m)" in {
    val f = ref(effectId("id"), fnType(stringType, stringType))
    val m = str
    val inner = app(ref(effectId("map"), fnType(stringType, idType(stringType))), f, GroundValue.Type)
    normalizedExpr(app(inner, m, idType(stringType)).value.expression)
      .shouldBe(MonomorphicExpression.FunctionApplication(f, m))
  }

  it should "erase nested Id wrappers runId(pure@Effect[Id](e)) to e" in {
    val pureApp = app(ref(effectId("pure"), fnType(stringType, idType(stringType))), str, idType(stringType))
    val runIdApp = app(ref(WellKnownTypes.runIdFQN, fnType(idType(stringType), stringType)), pureApp, stringType)
    normalizedExpr(runIdApp.value.expression).shouldBe(str.value.expression)
  }

  it should "leave a non-Id application untouched" in {
    val printLine = ValueFQN(ModuleName(Seq("eliot", "effect"), "Console"), QualifiedName("printLine", Qualifier.Ability("Console")))
    val call      = app(ref(printLine, fnType(stringType, stringType)), str, stringType)
    normalizedExpr(call.value.expression).shouldBe(call.value.expression)
  }

  it should "leave a pure@Effect[IO] application untouched (not the Id carrier)" in {
    val pureIo = ValueFQN(ModuleName(Seq("eliot", "jvm"), "IO"), QualifiedName("pure", Qualifier.AbilityImplementation("Effect", "IO")))
    val call   = app(ref(pureIo, fnType(stringType, stringType)), str, stringType)
    normalizedExpr(call.value.expression).shouldBe(call.value.expression)
  }

  it should "rewrite the runId accessor's own body to the identity via normalizeValue" in {
    val objName = at("obj")
    val body    = at[MonomorphicExpression.Expression](
      MonomorphicExpression.FunctionLiteral(objName, idType(stringType), node(stringType, MonomorphicExpression.StringLiteral(at("machinery"))))
    )
    IdNormalizer.normalizeValue(WellKnownTypes.runIdFQN, fnType(idType(stringType), stringType), body).value.shouldBe(
      MonomorphicExpression.FunctionLiteral(objName, idType(stringType), node(stringType, MonomorphicExpression.ParameterReference(objName)))
    )
  }

  it should "report Id-machinery references as residue before normalization and none after" in {
    val runIdApp = app(ref(WellKnownTypes.runIdFQN, fnType(idType(stringType), stringType)), str, stringType).value.expression
    IdNormalizer.residualIdReferences(runIdApp).shouldBe(Seq(WellKnownTypes.runIdFQN))
  }

  it should "report no residue after normalizing an applied runId" in {
    val runIdApp = app(ref(WellKnownTypes.runIdFQN, fnType(idType(stringType), stringType)), str, stringType).value.expression
    IdNormalizer.residualIdReferences(normalizedExpr(runIdApp)).shouldBe(Seq.empty)
  }

  "U1b type erasure" should "erase a top-level Id[X] to its payload" in {
    IdNormalizer.eraseIdTypes(idType(stringType)).shouldBe(stringType)
  }

  it should "erase a nested Id[X] inside another structure" in {
    IdNormalizer.eraseIdTypes(fnType(stringType, idType(stringType))).shouldBe(fnType(stringType, stringType))
  }

  it should "leave a bare unapplied Id carrier untouched" in {
    val bareId = GroundValue.Structure(WellKnownTypes.idFQN, Seq.empty, GroundValue.Type)
    IdNormalizer.eraseIdTypes(bareId).shouldBe(bareId)
  }

  it should "erase Id-headed node types and reference type-arguments in a body" in {
    val body    = app(ref(effectId("id"), fnType(idType(stringType), stringType)), str, idType(stringType))
    val erased  = IdNormalizer.eraseIdInBody(at(body.value.expression))
    IdNormalizer.hasResidualIdType(stringType, Some(erased)).shouldBe(false)
  }

  it should "detect a residual Id[X] type before erasure" in {
    val body = app(ref(effectId("id"), fnType(idType(stringType), stringType)), str, idType(stringType))
    IdNormalizer.hasResidualIdType(idType(stringType), Some(at(body.value.expression))).shouldBe(true)
  }

  // A first-class Id reference appears as a *child* node (an argument) in real code (a dot-chain `x.runId` lowers to
  // `_dot_(x, runId)`), where it carries its own function type — which the eta-expansion reads to build the lambda.
  private val dotFqn = ValueFQN(ModuleName(Seq("eliot", "lang"), "Function"), QualifiedName("_dot_", Qualifier.Default))

  "eta-expansion of a first-class reference" should "eta-expand a runId argument to a lambda" in {
    val call = app(ref(dotFqn, fnType(fnType(idType(stringType), stringType), stringType)),
                   ref(WellKnownTypes.runIdFQN, fnType(idType(stringType), stringType)), stringType)
    normalizedExpr(call.value.expression).shouldBe(a[MonomorphicExpression.FunctionApplication])
  }

  it should "leave no residual Id reference after eta-expanding a runId argument" in {
    val call = app(ref(dotFqn, fnType(fnType(idType(stringType), stringType), stringType)),
                   ref(WellKnownTypes.runIdFQN, fnType(idType(stringType), stringType)), stringType)
    IdNormalizer.residualIdReferences(normalizedExpr(call.value.expression)).shouldBe(Seq.empty)
  }

  it should "leave no residual Id reference after eta-expanding a flatMap@Effect[Id] argument" in {
    val fmType = fnType(fnType(stringType, idType(stringType)), fnType(idType(stringType), idType(stringType)))
    val call   = app(ref(dotFqn, fnType(fmType, stringType)), ref(effectId("flatMap"), fmType), stringType)
    IdNormalizer.residualIdReferences(normalizedExpr(call.value.expression)).shouldBe(Seq.empty)
  }

  it should "eta-expand a bare Id reference standing as the whole body, using the signature" in {
    val pureSig  = fnType(stringType, idType(stringType))
    val body     = at[MonomorphicExpression.Expression](ref(effectId("pure"), pureSig).value.expression)
    val aliasFqn = ValueFQN(ModuleName(Seq.empty, "Test"), QualifiedName("alias", Qualifier.Default))
    IdNormalizer.residualIdReferences(IdNormalizer.normalizeValue(aliasFqn, pureSig, body).value).shouldBe(Seq.empty)
  }
}
