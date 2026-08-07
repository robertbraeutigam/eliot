package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.processor.MetaConstructorDesugarer
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** **R2 enforcement** (docs/total-meta-transfers.md §2/§3/§P2): every **body-less** value — a native leaf — that
  * produces a meta-carrying type must **state** its meta transfer; a bodied value **derives** its transfer and states
  * nothing (R3's "may not state" is a later slice). This closes the TODO *"A native that produces a meta-carrying type
  * must state its meta-information"*: without it, a native returning e.g. `Int` (whose value range is meta-information)
  * and carrying no `{ ... }` transfer brace silently defaults its meta to ⊤ rather than being forced to say what it does.
  *
  * A rider on [[MonomorphicValue]] (runtime track), on the [[EffectAccountingProcessor]] template. The leaf test is
  * exactly the body test the fact already carries — `mv.runtime.isEmpty` (no Eliot body on this track); a bodied value
  * passes untouched. The return-meta test uses the value's **declared** (pre-monomorphization) return type
  * ([[OperatorResolvedValue]]), so the origination is distinguished from the pass-through:
  *
  *   - a **concrete return head** (`String::length : Int`) originates the meta — the leaf must state a `^Meta` transfer
  *     for it, or it is a compile error at the leaf;
  *   - a **type-parameter return head** (`foldLeftInternal[F[_], A, B] : F[B]`, `runId[A] : A`) does not originate a
  *     meta — it forwards whatever its argument/instantiation carries — so it is **exempt**. Deriving such a
  *     (higher-order) transfer is the meta-interpretation slice (§6.2/§P4), not this one.
  *
  * The check is normatively **use-site, post-mono** (§4): whether a return's meta is `Unit` is a per-instantiation fact,
  * so it rides each `MonomorphicValue`. The fact is produced only when the check passes; a violation aborts. Wiring that
  * abort as a [[WovenValue]] `getFactOrAbort` precondition — so it blocks codegen, a silently-defaulted meta never
  * reaching bytecode — is the arming step, deferred until the meta-carrying stdlib leaves state their transfers
  * (docs/total-meta-transfers.md §P2). Until then the processor is registered but undemanded (dormant).
  */
class MetaTransferAccountingProcessor
    extends TransformationProcessor[MonomorphicValue.Key, MetaTransferAccounting.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: MetaTransferAccounting.Key,
      mv: MonomorphicValue
  ): CompilerIO[MetaTransferAccounting] =
    verifyStated(mv).as(MetaTransferAccounting(key.vfqn, key.typeArguments))

  /** R2 for one instance: a **bodied** value derives (nothing to state); a **body-less** leaf whose declared return head
    * is a concrete meta-carrying type must declare a `^Meta` transfer, else it is reported at the value. A
    * type-parameter return head (a forwarded, non-originated meta) is exempt.
    */
  private def verifyStated(mv: MonomorphicValue): CompilerIO[Unit] =
    if (mv.runtime.isDefined) ().pure[CompilerIO]
    else
      getFactIfProduced(OperatorResolvedValue.Key(mv.vfqn, Platform.Runtime)).flatMap {
        case None      => ().pure[CompilerIO]
        case Some(orv) =>
          returnHead(OperatorResolvedExpression.SignatureView.of(orv.signature).returnType.value) match {
            case OperatorResolvedExpression.ValueReference(typeName, _) =>
              for {
                metaCarrying   <- declaresMetaStructure(typeName.value)
                statesTransfer <- declaresTransfer(mv.vfqn)
                _              <- if (metaCarrying && !statesTransfer) reportMissing(mv, typeName.value)
                                  else ().pure[CompilerIO]
              } yield ()
            case _                                                     =>
              // A type-parameter (or literal) return head: the meta is forwarded from an argument/instantiation, not
              // originated here — exempt, its (higher-order) transfer is the meta-interpretation slice (§P4).
              ().pure[CompilerIO]
          }
      }

  /** The ultimate application head of a (return) type expression — `F[B]` ⤳ `F`, `Int` ⤳ `Int`. */
  private def returnHead(expr: OperatorResolvedExpression): OperatorResolvedExpression = expr match {
    case OperatorResolvedExpression.FunctionApplication(target, _) => returnHead(target.value)
    case other                                                     => other
  }

  /** Whether a concrete type declares meta-information — i.e. its module (in the compiler pool, where
    * [[MetaConstructorDesugarer]] emits meta structures) declares its `T$Meta` structure. A slotless type declares none,
    * so its meta is the trivial `Unit` and a leaf returning it needs no transfer.
    */
  private def declaresMetaStructure(typeName: ValueFQN): CompilerIO[Boolean] =
    getFactOrAbort(UnifiedModuleNames.Key(typeName.moduleName, Platform.Compiler)).map(
      _.names.contains(
        QualifiedName(typeName.name.name + MetaConstructorDesugarer.metaTypeSuffix, Qualifier.Type)
      )
    )

  /** Whether this value declares a `^Meta` transfer — its own name in the [[Qualifier.Meta]] namespace of its module
    * (the compiler pool), exactly what [[com.vanillasource.eliot.eliotc.core.processor.MetaTransferDesugarer]] emits
    * from a `{ ... }` return brace (and what [[RefinementChannelProcessor.metaCompanionFqn]] reads). Keeping only
    * `name.name` also matches an ability-impl leaf's stripped companion (`Numeric[Int]::add` ⤳ `add^Meta`).
    */
  private def declaresTransfer(vfqn: ValueFQN): CompilerIO[Boolean] =
    getFactOrAbort(UnifiedModuleNames.Key(vfqn.moduleName, Platform.Compiler)).map(
      _.names.contains(QualifiedName(vfqn.name.name, Qualifier.Meta))
    )

  private def reportMissing(mv: MonomorphicValue, typeName: ValueFQN): CompilerIO[Unit] =
    compilerAbort[Unit](
      mv.name.as(
        s"This native value produces '${typeName.name.name}', which carries meta-information, but states no meta " +
          s"transfer; add a { ... } return brace saying what it does to the meta-information."
      )
    )
}
