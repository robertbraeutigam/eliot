package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.processor.MetaConstructorDesugarer
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** The **native meta-declaration check** — a post-monomorphization rider on [[MonomorphicValue]], built on the same
  * template as [[RefinementChannelProcessor]] and [[EffectAccountingProcessor]]. It enforces one rule: a **native**
  * value (a body-less one — its reduction is a platform leaf, not an Eliot body the channel can look through) whose
  * ground return type carries **non-`Unit` refinement meta** must declare a `^Meta` transfer companion (a
  * return-position `{…}` brace, [[com.vanillasource.eliot.eliotc.core.processor.MetaTransferDesugarer]]).
  *
  * '''Why only natives.''' The refinement channel is intra-procedural: at a call it reads the callee's `^Meta`
  * companion and, absent one, records ⊤ (a bignum layout — sound but wide). For an ordinary bodied def that ⊤ is
  * merely imprecise; its meta *could* be re-derived from its body later. A native has no body to look through, so its
  * companion is the *only* place its range can ever come from — and silently defaulting a meta-carrying native to ⊤
  * hides that the platform never said what the native does to the range. This check turns that silent default into a
  * hard error, so a native producing a value of a meta-carrying type has to state its meta every time (e.g.
  * `def exitCode(result: ProcessResult): Int {Interval(0, 255)}`, or `Int {unbounded}` where the range is genuinely
  * the whole domain).
  *
  * '''What it does not reach, by design.''' The check fires on the ground return **head**: an effect-carrier-wrapped
  * return (`{Console} Int`, whose mono return is `IO[Int]`) is *not* a bare meta-carrying type and is exempt — the
  * channel is already ⊤ through a carrier, and no mechanism yet declares a transfer across one. A generic native whose
  * return only becomes meta-carrying at a concrete instantiation is checked at that instantiation (use-site
  * verification), exactly like every other post-mono channel; the companion need only *exist* (a generic `^Meta` such
  * as `integerLiteral^Meta` covers every instantiation).
  *
  * Produced only when the rule holds; the abort on a violation blocks the value's [[WovenValue]] (its `getFactOrAbort`
  * precondition) and so its codegen, the same fail-safe wiring [[EffectAccountingProcessor]] uses.
  */
class NativeMetaDeclarationProcessor
    extends TransformationProcessor[MonomorphicValue.Key, NativeMetaDeclared.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: NativeMetaDeclared.Key,
      mv: MonomorphicValue
  ): CompilerIO[NativeMetaDeclared] = {
    val checkNative =
      if (mv.runtime.isDefined) ().pure[CompilerIO] // a bodied def: its ⊤ is imprecise, not a silent gap
      else
        for {
          metaCarrying <- returnIsMetaCarrying(mv)
          produces     <- returnIsConcretelyDeclared(mv.vfqn)
          declared     <- hasMetaCompanion(mv.vfqn)
          _            <- reportMissingMeta(mv).whenA(metaCarrying && produces && !declared)
        } yield ()
    checkNative.as(NativeMetaDeclared(key.vfqn, key.typeArguments))
  }

  /** Whether this value's ground return type is a **meta-carrying** type — a type declaring a `$Meta` structure (a
    * slotted type like `Int`), the exact predicate the channel uses to decide a type has a non-`Unit` meta
    * ([[RefinementChannelProcessor.metaTypeOf]]). `Id` is erased first (a pure value's return still rides the identity
    * carrier at this seam), then the function type's deep return head is taken; a non-structure head (a generic binder,
    * `Type`) is never meta-carrying.
    */
  private def returnIsMetaCarrying(mv: MonomorphicValue): CompilerIO[Boolean] =
    IdNormalizer.eraseIdTypes(mv.signature).deepReturnType match {
      case GroundValue.Structure(fqn, _, _) =>
        val metaName = QualifiedName(fqn.name.name + MetaConstructorDesugarer.metaTypeSuffix, Qualifier.Type)
        getFactIfProduced(UnifiedModuleNames.Key(fqn.moduleName, Platform.Compiler))
          .map(_.exists(_.names.contains(metaName)))
      case _                                => false.pure[CompilerIO]
    }

  /** Whether the value's **own declaration** fixes its return to a concrete type — as opposed to forwarding a **plain
    * generic** payload (`def foldLeftInternal[A, B](…): B`, the `fold`/`match` eliminators). By effects-as-rows rule 4 a
    * plain generic return is a payload the value transports, never one it *produces*: its refinement flows in from the
    * caller's argument (tracked, where at all, by a `^Meta` **merge** companion like `fold^Meta`), so the value has
    * nothing of its own to state and must not be forced to. This is read off the **abstract** signature — where a
    * generic return is a bound binder (`ParameterReference`) and a produced one is a concrete type (`ValueReference`) —
    * because the ground return alone cannot tell `B := Int` apart from a literal `Int`.
    *
    * An effect-carrier-wrapped return (`{Process} Int`) is already exempt upstream by the *ground* meta-carrying test
    * ([[returnIsMetaCarrying]]): its mono return head is the carrier, not the bare `Int`. So this need only separate the
    * bare-generic payload from the concrete producer.
    */
  private def returnIsConcretelyDeclared(vfqn: ValueFQN): CompilerIO[Boolean] =
    getFactIfProduced(OperatorResolvedValue.Key(vfqn, Platform.Runtime)).map {
      case Some(orv) =>
        applicationHead(OperatorResolvedExpression.SignatureView.of(orv.signature).returnType.value) match {
          case _: OperatorResolvedExpression.ValueReference => true
          case _                                            => false
        }
      case None      => true
    }

  @scala.annotation.tailrec
  private def applicationHead(expr: OperatorResolvedExpression): OperatorResolvedExpression =
    expr match {
      case OperatorResolvedExpression.FunctionApplication(target, _) => applicationHead(target.value)
      case other                                                    => other
    }

  /** Whether the value declares a `^Meta` transfer companion — the same cheap [[UnifiedModuleNames]] membership test the
    * channel uses to recognise a refinement transfer/merge ([[RefinementChannelProcessor.metaViaCompanion]]).
    */
  private def hasMetaCompanion(vfqn: ValueFQN): CompilerIO[Boolean] =
    getFactIfProduced(UnifiedModuleNames.Key(vfqn.moduleName, Platform.Compiler))
      .map(_.exists(_.names.contains(QualifiedName(vfqn.name.name, Qualifier.Meta))))

  private def reportMissingMeta(mv: MonomorphicValue): CompilerIO[Unit] =
    compilerAbort[Unit](
      mv.name.as(
        s"The native '${mv.vfqn.name.name}' produces a value of a type that carries refinement meta-information but does " +
          "not declare what range it produces; add a return brace stating its meta (e.g. `: Int {Interval(0, 255)}`, or " +
          "`: Int {unbounded}` when the range is the whole domain)."
      )
    )
}
