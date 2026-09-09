package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.processor.EffectMachinery
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.row.BindingWriter
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** The **post-monomorphization effect accounting** (`docs/effects.md` §9.4, F6) — a rider on [[MonomorphicValue]]
  * built on the same template as [[RefinementChannelProcessor]], and a codegen precondition through
  * [[WovenValueProcessor]]'s `getFactOrAbort`.
  *
  * Under effects v6 it reads **bindings**, not carriers. An instantiation *receives* one implementation per phantom
  * binder of its own signature ([[BindingWriter.phantoms]]) — the mono key's arguments at those indices. A reference
  * in its body *forwards* one of them when the binding written at that reference's own phantom slot is, by exact
  * [[GroundValue]] equality, the very implementation this value received for that ability. The union of the abilities
  * so forwarded is the derived row, and the check is `derived ⊆ declared` against the value's declared row — the same
  * diagnostic, in the same vocabulary, as before.
  *
  * '''What this is, and what it is not (D7).''' The verifier under v6 is the **pre-mono scope check**, which is the
  * write's own walk ([[BindingWriter]]): an effect with no binding in lexical scope is reported at the reference, at
  * the definition, before anything downstream runs. What is left here is that check's post-mono shadow — "the
  * bindings actually consulted ⊆ the row declared", where a consulted binding exists only because the declaration
  * minted a binder for it. It is expected to fire on nothing, and it is kept through the flag day to be *traced*
  * rather than argued about (§10.3 A5): every non-empty derivation is logged, and it retires when the trace is empty
  * over the corpus. Not before, and not on the argument alone.
  */
class EffectAccountingProcessor
    extends TransformationProcessor[MonomorphicValue.Key, EffectAccounting.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: EffectAccounting.Key,
      mv: MonomorphicValue
  ): CompilerIO[EffectAccounting] =
    for {
      orv     <- getFactIfProduced(OperatorResolvedValue.Key(mv.vfqn, Platform.Runtime))
      derived <- orv.fold(Set.empty[AbilityFQN].pure[CompilerIO])(derivedRow(mv, _))
      _       <- debug[CompilerIO](s"effect accounting derived $derived for ${mv.vfqn}").whenA(derived.nonEmpty)
      _       <- orv.traverse_(verifySubset(mv, _, derived))
    } yield EffectAccounting(key.vfqn, key.typeArguments, derived)

  /** The abilities this instantiation forwards: for every reference in its body, every phantom binder of that callee
    * whose written argument *is* one of the implementations this value received, and which the callee declares as a
    * row entry.
    *
    * The last clause is where effect-ness is read, and it is read in exactly one place — the callee's declared row
    * ([[BindingWriter]]'s rule). A `~` constraint's binding is forwarded the same way and is not an effect, so it
    * contributes nothing.
    */
  private def derivedRow(mv: MonomorphicValue, orv: OperatorResolvedValue): CompilerIO[Set[AbilityFQN]] = {
    val received = receivedBindings(mv.typeArguments, orv)
    if (received.isEmpty) Set.empty[AbilityFQN].pure[CompilerIO]
    else
      mv.runtime.fold(Set.empty[AbilityFQN].pure[CompilerIO]) { body =>
        collectReferences(body.value).toList.foldLeftM(Set.empty[AbilityFQN]) { case (acc, (ref, typeArgs)) =>
          forwardedAt(ref, typeArgs, received).map(acc ++ _)
        }
      }
  }

  /** The implementations an instantiation received, by ability: its own phantom binders' arguments off the mono key.
    * A binder the key does not reach (a partially applied key) is skipped rather than guessed.
    */
  private def receivedBindings(
      typeArguments: Seq[GroundValue],
      orv: OperatorResolvedValue
  ): Map[AbilityFQN, GroundValue] =
    BindingWriter.phantoms(orv).flatMap { case (index, ability) =>
      typeArguments.lift(index).map(ability -> _)
    }.toMap

  /** The effects one reference forwards. Reads the callee's declaration through `getFactIfProduced`: a callee whose
    * own definition aborted upstream contributes nothing, which is the direction that cannot invent an effect.
    */
  private def forwardedAt(
      ref: ValueFQN,
      typeArgs: Seq[GroundValue],
      received: Map[AbilityFQN, GroundValue]
  ): CompilerIO[Set[AbilityFQN]] =
    getFactIfProduced(OperatorResolvedValue.Key(ref, Platform.Runtime)).map {
      case None         => Set.empty
      case Some(callee) =>
        val declared = declaredRow(callee)
        BindingWriter
          .phantoms(callee)
          .collect {
            case (index, ability)
                if declared.contains(ability) && typeArgs.lift(index).exists(received.get(ability).contains) =>
              ability
          }
          .toSet
    }

  /** A value's **declared row**: the entries of its return row, machinery excluded. The one place effect-ness is
    * written down (`docs/effects.md` §9.4 — an `effect`'s members get `{X}`, an ordinary definition gets what its
    * `{ … }` says); a `~` constraint's ability is in no row and so is not an effect.
    */
  private def declaredRow(orv: OperatorResolvedValue): Set[AbilityFQN] =
    orv.effectRow.returnEffects
      .map(_.abilityFQN)
      .filterNot(a => EffectMachinery.isMachineryAbility(a.abilityName))
      .toSet

  /** `derived ⊆ declared`. An undeclared effect is reported at the value and the accounting **declines (aborts)**,
    * which [[WovenValueProcessor]]'s `getFactOrAbort` precondition turns into blocked code generation.
    */
  private def verifySubset(
      mv: MonomorphicValue,
      orv: OperatorResolvedValue,
      derived: Set[AbilityFQN]
  ): CompilerIO[Unit] = {
    val undeclared = derived.diff(declaredRow(orv))
    if (undeclared.isEmpty) ().pure[CompilerIO] else reportUndeclared(mv, undeclared)
  }

  /** Every value reference in a monomorphic body with its ground type arguments, in traversal order (parameter
    * references and literals excluded). The `typeArguments` are the reference's own mono key, which is where its
    * written bindings sit.
    */
  private def collectReferences(expr: MonomorphicExpression.Expression): Seq[(ValueFQN, Seq[GroundValue])] =
    expr match {
      case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) => Seq((vfqn.value, typeArgs))
      case MonomorphicExpression.FunctionApplication(target, arg)          =>
        collectReferences(target.value.expression) ++ collectReferences(arg.value.expression)
      case MonomorphicExpression.FunctionLiteral(_, _, body)               => collectReferences(body.value.expression)
      case _                                                               => Seq.empty
    }

  private def reportUndeclared(mv: MonomorphicValue, undeclared: Set[AbilityFQN]): CompilerIO[Unit] = {
    val names   = undeclared.toSeq.map(_.abilityName).sorted
    val word    = if (names.sizeIs == 1) "effect" else "effects"
    val pronoun = if (names.sizeIs == 1) "it" else "them"
    compilerAbort[Unit](
      mv.name.as(
        s"This value performs the $word ${names.map(n => s"'$n'").mkString(", ")} but does not declare $pronoun; " +
          s"add $pronoun to its { ... } effect set."
      )
    )
  }
}
