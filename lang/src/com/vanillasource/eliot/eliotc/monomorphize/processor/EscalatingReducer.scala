package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.{Env, MetaStore, SemValue}
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, MonomorphicEvaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

/** The compiler platform's post-monomorphize **linker-executor** — the `used`-equivalent the runtime platform already has
  * (`docs/refinement-channel-transfer-reduction.md` §3). It evaluates a compiler-track value applied to concrete
  * argument metas and enforces the invariant that *post-monomorphize evaluation may only ever see monomorphized bodies*:
  * a raw (operator-resolved) callee body may appear only beneath a native leaf, never spliced into the reduction. The
  * cheap common case (a transfer bottoming at natives) pays nothing new; when the one-hop raw closure sticks on an
  * ability reference that a monomorphization would have exchanged for its exact impl, the loop *escalates* — it re-links
  * each stuck callee's own monomorphized (reduced-at-instantiation) form and re-evaluates.
  *
  * The stuck-driven loop skeleton ([[escalatingLoop]] + [[reducibleStuck]] + [[escalate]]) is the **one** mechanism the
  * in-checker read-back
  * ([[com.vanillasource.eliot.eliotc.monomorphize.check.PostDrainQuoter]]) and this channel-facing executor share: each
  * instantiates it with its own (eval function, escalation fetch), so they are one mechanism, not siblings. Each fetched
  * reduced form was itself produced by a read-back that ran this same loop, so the recursion lives in the cached,
  * `activeFactKeys`-guarded fact graph rather than in a closure-composition flag.
  */
object EscalatingReducer {

  /** Reduce compiler-track value `vfqn` at concrete `typeArguments`, applied to `argMetas`, to a ground value — linking
    * only monomorphized callees.
    *
    *   1. fetch `CompilerMonomorphicValue(vfqn, typeArguments)`'s reduced body;
    *   1. seed the bindings with the one-hop raw closure ([[ReducedBindingClosure.collectBindings]], `deep = false`) — the
    *      cheap common case (a transfer bottoming at natives) pays nothing new;
    *   1. evaluate the reduced body, apply `argMetas`, and `renormalize` (re-fires natives; empty metastore);
    *   1. if the result quotes, done; if it is *reducibly* stuck, escalate — re-link each of the body's value references
    *      **reduced at its own ground type arguments** and re-evaluate; loop until it quotes or no new binding is
    *      available.
    *
    * [[None]] when the value produced no `CompilerMonomorphicValue`, has no reduced body, or the final result does not
    * reduce to a quotable ground form.
    */
  def reduceApplied(
      vfqn: ValueFQN,
      typeArguments: Seq[GroundValue],
      argMetas: Seq[SemValue]
  ): CompilerIO[Option[GroundValue]] =
    getFactIfProduced(CompilerMonomorphicValue.Key(vfqn, typeArguments)).flatMap {
      case Some(CompilerMonomorphicValue(_, _, _, _, Some(reduced))) =>
        ReducedBindingClosure.collectBindings(reduced.value, vfqn, Platform.Compiler, deep = false).flatMap { base =>
          def evalWith(extra: Map[ValueFQN, SemValue]): SemValue = {
            val lookup: ValueFQN => Option[SemValue] = fqn => extra.get(fqn).orElse(base.get(fqn))
            val body                                 =
              new MonomorphicEvaluator(lookup).eval(Env.empty, MonomorphicExpression(GroundValue.Type, reduced.value))
            val applied                              = argMetas.foldLeft(body)(Evaluator.applyValue)
            Evaluator.renormalize(applied, MetaStore.empty, lookup, deep = true)
          }

          val candidates = ReducedBindingClosure.valueReferences(reduced.value).toSeq.filterNot(_._1 == vfqn)
          escalatingLoop(
            MetaStore.empty,
            evalWith,
            already => escalate(candidates, already, ReducedBindingClosure.reduceInstance(_, _, deep = true), (_, sem) => sem)
          ).map(result => Quoter.quote(0, result, MetaStore.empty).toOption)
        }
      case _                                                        => none[GroundValue].pure[CompilerIO]
    }

  /** The shared stuck-driven escalation loop. Evaluate with the current escalated bindings; if the forced result quotes
    * (against `metaStore`) return it — the common case pays nothing. If it is [[reducibleStuck]], `fetch` more bindings
    * (keyed by what has already been escalated) and loop until it quotes or no new binding is available.
    */
  def escalatingLoop(
      metaStore: MetaStore,
      eval: Map[ValueFQN, SemValue] => SemValue,
      fetch: Set[ValueFQN] => CompilerIO[Map[ValueFQN, SemValue]]
  ): CompilerIO[SemValue] = {
    def loop(bindings: Map[ValueFQN, SemValue]): CompilerIO[SemValue] = {
      val forced = eval(bindings)
      if (!reducibleStuck(forced, metaStore)) forced.pure[CompilerIO]
      else
        fetch(bindings.keySet).flatMap { extra =>
          if (extra.isEmpty) forced.pure[CompilerIO] else loop(bindings ++ extra)
        }
    }
    loop(Map.empty)
  }

  /** Whether a *forced* result is stuck in a way escalation could resolve. A runtime `VLam` (a compiler-track function
    * over its value parameters) and a value-parameter / read-back neutral are legitimately structural — they must fall
    * straight through to the caller, never re-fire escalation. Anything else that fails to quote (a bodied top-def, a
    * stuck `match`, a stuck native) is a candidate.
    */
  def reducibleStuck(forced: SemValue, metaStore: MetaStore): Boolean = forced match {
    case SemValue.VLam(_, _)                                    => false
    case SemValue.VNeutral(SemValue.NeutralHead.Param(_, _), _) => false
    case SemValue.VNeutral(SemValue.NeutralHead.Fresh(_, _), _) => false
    case other                                                  => Quoter.quote(0, other, metaStore).isLeft
  }

  /** Fetch the *reduced-at-instantiation* binding for each `candidate` value reference not yet escalated (`already`),
    * with non-empty ground type arguments, and not an ancestor on the active fact-request chain (would dead-lock). Each
    * fetched form is post-processed by `wrap` (the in-checker read-back wraps in ignore-lambdas to absorb the reference's
    * type arguments; the [[MonomorphicEvaluator]] path drops type arguments already, so it passes the reduced form
    * through unchanged). Keyed by FQN; one instantiation per FQN.
    */
  def escalate(
      candidates: Seq[(ValueFQN, Seq[GroundValue])],
      already: Set[ValueFQN],
      reduceInstance: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[SemValue]],
      wrap: (Int, SemValue) => SemValue
  ): CompilerIO[Map[ValueFQN, SemValue]] =
    activeFactKeys.flatMap { ancestors =>
      candidates.distinctBy(_._1).foldLeftM(Map.empty[ValueFQN, SemValue]) { case (acc, (fqn, groundArgs)) =>
        if (
          already.contains(fqn) || groundArgs.isEmpty ||
          ancestors.contains(CompilerMonomorphicValue.Key(fqn, groundArgs))
        )
          acc.pure[CompilerIO]
        else
          reduceInstance(fqn, groundArgs).map {
            case Some(sem) => acc + (fqn -> wrap(groundArgs.size, sem))
            case None      => acc
          }
      }
    }
}
