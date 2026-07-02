package com.vanillasource.eliot.eliotc.monomorphize.refine

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.check.SemExpression
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.unify.UnifyResult
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The refinement-bounds solver (D4): the type system's *refinement lattice*, factored out of the
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker]] into one cohesive module so the two genuinely
  * different relations the system uses are no longer entangled.
  *
  *   - **Definitional equality** is the
  *     [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier]]'s job: force both sides, compare, solve metas.
  *   - **The refinement lattice** is this module's job: the directional `Coerce` subtyping relation (check-mode
  *     widening), the `Combine` join (least-upper-bound of covariant multi-candidate metavariables), and the deferred
  *     "result fits expected" upper-bound obligations. None of these is equality-shaped; they were the four
  *     non-equality features the architecture review identified as the churn source when hosted inside the equality
  *     core.
  *
  * The public interface is small:
  *   - [[unifyOrCoerce]] — the check-mode entry point: try definitional equality, else a `Coerce` insertion.
  *   - [[resolveCombines]] — a saturation pass that joins each combinable meta's accumulated candidates.
  *   - [[resolveUpperBounds]] — the finalization pass discharging deferred upper-bound obligations.
  *
  * The combinable / candidate / taint *data* still lives in the [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier]]'s
  * [[com.vanillasource.eliot.eliotc.monomorphize.domain.MetaRole]] map, and the *interception* that accumulates a
  * candidate instead of failing stays in `Unifier.unify` — it is interleaved with the recursive definitional-equality
  * descent (a contribution can arrive at any nesting level), so it is genuinely a unification-time decision, not an
  * algorithm step that can be hoisted out. This module owns the *algorithm* that interprets that data as a lattice.
  *
  * Operates over [[com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO]], reading and writing the shared
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.CheckState]] through `get`/`modify`/`inspect`. It depends on
  * exactly five checker primitives, passed at construction — this narrow, explicit surface is what makes the module
  * boundary real.
  *
  * @param resolveAbility
  *   Resolve an ability instance by name and ground type arguments (the `Coerce` / `Combine` instance lookup).
  * @param evalExpr
  *   Evaluate an ORE expression against an env (prefetching every reachable binding) — the checker's `evalExpr`.
  * @param force
  *   Force a SemValue through the current meta store — the checker's `force`.
  * @param freshMeta
  *   Allocate a fresh metavariable — the checker's `freshMeta`.
  * @param doUnify
  *   Unify two semantic values, updating the unifier in the state — the checker's `doUnify`.
  */
class RefinementSolver(
    resolveAbility: (ValueFQN, Seq[GroundValue], Platform) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
    evalExpr: (OperatorResolvedExpression, Option[Env]) => CheckIO[SemValue],
    force: SemValue => CheckIO[SemValue],
    freshMeta: CheckIO[VMeta],
    doUnify: (SemValue, SemValue, Sourced[String]) => CheckIO[Unit],
    platform: Platform
) {

  /** Resolve a term against an expected type. First attempt pure definitional equality (the unifier). If that fails,
    * attempt a check-mode `Coerce` insertion: where the inferred type is used where a different expected type built
    * from the same constructor is wanted (e.g. `Int[3,3]` where `Int[0,10]` is expected), resolve the user-defined
    * `Coerce` ability by name and evaluate its `coerce` through the one NbE evaluator. A `some payload` result means
    * the coercion exists, and the `some` payload is spliced as a real conversion node wrapping the term (see
    * [[buildCoercedExpr]]) — for `Int` that is a `nativeWiden` the backend lowers to an unbox→rebox between the source
    * and target representations. A `none` or stuck result means no coercion, so the original "Type mismatch." is
    * committed.
    *
    * `unify` itself stays pure definitional equality — the directional widening lives here, in check mode only.
    */
  def unifyOrCoerce(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue
  ): CheckIO[SemExpression] =
    for {
      s      <- get
      result <- s.unifier.tryUnify(actual, expected, tm.as("Type mismatch.")) match {
                  case UnifyResult.Unified(u)       => modify(_.withUnifier(u)).as(expr)
                  case UnifyResult.Contradiction(_) =>
                    tryCoerce(tm, expr, actual, expected).flatMap {
                      case Some(coerced) => pure(coerced)
                      // No coercion: report a single mismatch (carrying Expected/Actual) at the term rather than
                      // committing the failed unification, whose spine descent can yield one error per mismatched
                      // type argument (e.g. both `Int` bounds).
                      case None          =>
                        modify(st => st.withUnifier(st.unifier.addMismatch(actual, expected, tm.as("Type mismatch."))))
                          .as(expr)
                    }
                }
    } yield result

  /** Try to resolve and apply a `Coerce[actual, expected]` instance. Returns the coerced expression on success (the
    * resolved `coerce`'s `some` payload spliced around the term — see [[buildCoercedExpr]]), or `None` when no coercion
    * applies (no instance, a `none` result, or bounds too abstract to decide). Only fires for leaf positions (argument
    * / binding / return); coercing inside a constructor (e.g. `List[Int[0,5]] → List[Int[0,10]]`) needs variance
    * reasoning and is not handled here.
    */
  private def tryCoerce(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue
  ): CheckIO[Option[SemExpression]] =
    resolveCoercionPayload(actual, expected, tm.as("Type mismatch.")).flatMap {
      case None          => pure(None)
      case Some(payload) => buildCoercedExpr(tm, expr, actual, expected, payload).map(Some(_))
    }

  /** Sentinel name of the neutral variable [[coercionPayload]] binds the `coerce` argument to, so [[buildCoercedExpr]]
    * can recognise it as the slot the actual expression substitutes into.
    */
  private val coerceArgName = "$coerceArg"

  /** Build the coerced expression from the resolved `Coerce` instance's `some` payload (the value the coercion yields,
    * with the `coerce` argument left as the [[coerceArgName]] marker). This is the principled Phase-5 splice: rather
    * than re-typing the term (which loses the source representation and produces a wrong-width value at runtime), the
    * payload is materialised as a real conversion node wrapping the original expression.
    *
    *   - A payload of the shape `conv[…](marker)` — a single body-less conversion native applied to the coerce argument
    *     — splices `conv(expr)` typed at `expected`, leaving `expr` at its narrower `actual` type. The conversion's own
    *     type arguments are the source type's arguments followed by the target type's (for `Int`: `[Smin, Smax, Tmin,
    *     Tmax]`), so the monomorphic `conv` reference is well-typed; the JVM backend reads the source/target
    *     representations from the argument and result types, not these arguments.
    *   - Any other payload (an identity coercion whose payload is the bare marker, or a shape this splice does not
    *     model) falls back to re-typing the term at `expected`.
    *
    * No `Int`-specific knowledge: the conversion FQN comes from the resolved instance and the type arguments from the
    * source/target type constructors.
    */
  private def buildCoercedExpr(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue,
      payload: SemValue
  ): CheckIO[SemExpression] =
    force(payload).flatMap {
      case VTopDef(convFqn, None, spine) if isCoerceArgMarker(spine.toList.lastOption) =>
        for {
          actualArgs   <- typeConstructorArgs(actual)
          expectedArgs <- typeConstructorArgs(expected)
        } yield {
          val convRef = SemExpression(
            VPi(actual, _ => expected),
            SemExpression.ValueReference(tm.as(convFqn), actualArgs ++ expectedArgs)
          )
          SemExpression(expected, SemExpression.FunctionApplication(tm.as(convRef), tm.as(expr)))
        }
      case _                                                                           =>
        pure(expr.copy(expressionType = expected))
    }

  private def isCoerceArgMarker(v: Option[SemValue]): Boolean = v match {
    case Some(VNeutral(NeutralHead.VVar(_, name), Spine.SNil)) => name == coerceArgName
    case _                                                     => false
  }

  /** The (forced) type arguments of a type-constructor value, e.g. `[Smin, Smax]` of `Int[Smin, Smax]`. A
    * non-constructor head yields no arguments — the caller then splices a conversion with no type arguments, which only
    * happens for coercions between argument-less types.
    */
  private def typeConstructorArgs(v: SemValue): CheckIO[Seq[SemValue]] =
    force(v).map {
      case VTopDef(_, None, spine) => spine.toList
      case _                       => Seq.empty
    }

  /** Resolve `Coerce[actual, expected]` by name and report whether the coercion exists (the resolved `coerce` evaluates
    * to `some`). This is the shared core of the check-mode insertion ([[tryCoerce]]) and the `Combine` contributor
    * check ([[coercionExists]]) — both need "does this coercion hold?" but differ in what they do with the answer. It
    * does **not** attempt definitional equality first (callers decide whether to); it returns `false` when the bounds
    * are abstract/unsolved (cannot quote to ground), when no instance resolves, or when the instance yields `none`.
    */
  private def resolveCoercion(actual: SemValue, expected: SemValue, context: Sourced[String]): CheckIO[Boolean] =
    resolveCoercionPayload(actual, expected, context).map(_.isDefined)

  /** Resolve `Coerce[actual, expected]` by name and return the resolved instance's `some` payload (the value the
    * coercion yields, with the `coerce` argument left as the [[coerceArgName]] marker), or [[None]] when no coercion
    * applies (abstract/unsolved bounds, no instance, or a `none` result). The shared core of [[tryCoerce]] (which
    * splices the payload) and [[resolveCoercion]] (which only needs existence).
    */
  private def resolveCoercionPayload(
      actual: SemValue,
      expected: SemValue,
      context: Sourced[String]
  ): CheckIO[Option[SemValue]] =
    for {
      s      <- get
      grounds = for {
                  a <- Quoter.quote(0, actual, s.unifier.metaStore)
                  e <- Quoter.quote(0, expected, s.unifier.metaStore)
                } yield (a, e)
      result <- grounds match {
                  case Left(_)         => pure(None) // abstract / unsolved bounds — cannot prove a coercion
                  case Right((aG, eG)) =>
                    liftF(resolveAbility(WellKnownTypes.coerceFQN, Seq(aG, eG), Platform.Runtime)).flatMap {
                      case None               => pure(None)
                      case Some((implFqn, _)) => coercionPayload(implFqn, actual, expected, context)
                    }
                }
    } yield result

  /** Evaluate the resolved `coerce` implementation against the concrete bounds and return its `some` payload (the value
    * the coercion yields), or [[None]] when it yields `none`. The `coerce` argument is bound to a [[coerceArgName]]
    * marker neutral, so the payload carries that marker wherever the runtime value flows — [[buildCoercedExpr]] splices
    * the actual expression in its place. A `Some(payload)` result therefore both proves the coercion exists and
    * provides the conversion to splice.
    *
    * The impl's generic bound parameters are not lambdas in its runtime body (only value parameters are — see
    * `CoreExpressionConverter.buildCurriedBody`); they are free names resolved through the env. So:
    *   - peel the impl signature's type-parameter `VLam`s into fresh metas (recording each `name → meta`);
    *   - unify the resulting value-level function type against `VPi(actual, _ => Option[expected])` and drain, which
    *     solves those metas to the concrete bounds;
    *   - rebuild the body env binding each name to the **forced (concrete)** meta value — not the meta itself: the NbE
    *     evaluator has no metastore, so a native applied to an unforced `VMeta` would go stuck during evaluation.
    *
    * Evaluating the runtime body in that concrete env and forcing it reduces the bounds-only existence check to
    * `some`/`none`. Coercion runs user code at compile time, which Girard's paradox allows to diverge; the termination
    * guard is deferred with the recursion/effect model.
    */
  private def coercionPayload(
      implFqn: ValueFQN,
      actual: SemValue,
      expected: SemValue,
      context: Sourced[String]
  ): CheckIO[Option[SemValue]] =
    for {
      orvOpt <- liftF(getFact(OperatorResolvedValue.Key(implFqn, platform)))
      result <- orvOpt.flatMap(orv => orv.runtime.map((orv, _))) match {
                  case None              => pure(Option.empty[SemValue])
                  case Some((orv, body)) =>
                    for {
                      sigSem        <- evalExpr(orv.typeStack.value.signature, Some(Env.empty))
                      (mono, binds) <- peelSigMetas(sigSem, Seq.empty)
                      optionExpected =
                        Evaluator.applyValue(VTopDef(WellKnownTypes.optionFQN, None, Spine.SNil), expected)
                      _             <- doUnify(mono, VPi(actual, _ => optionExpected), context)
                      _             <- modify(st => st.withUnifier(st.unifier.drain()))
                      concreteEnv   <- binds.foldLeftM(Env.empty) { case (env, (name, meta)) =>
                                         force(meta).map(env.bind(name, _))
                                       }
                      bodyVal       <- evalExpr(body.value, Some(concreteEnv))
                      applied        = Evaluator.applyValue(bodyVal, VNeutral(NeutralHead.VVar(0, coerceArgName), Spine.SNil))
                      forced        <- force(applied)
                    } yield forced match {
                      // `some payload` ⟹ coercion holds; the payload is the value argument of `some`.
                      case VTopDef(fqn, _, spine) if fqn == WellKnownTypes.someFQN => spine.toList.lastOption
                      case _                                                       => None
                    }
                }
    } yield result

  /** Resolve every combinable metavariable that accumulated more than one distinct candidate type
    * ([[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.candidatesOf]]). For a still-combinable (covariant)
    * meta the candidates are folded pairwise through the `Combine` ability into their join `R`; the meta is solved to
    * `R` and each contributor is verified to `Coerce` into `R` (always true for `Int` by construction). When no
    * `Combine` instance applies — or the meta was tainted by a contravariant use — the candidates are re-unified
    * strictly, which surfaces the ordinary "Type mismatch." that first-candidate-wins would have produced. Returns
    * whether any meta was newly resolved, so the caller can iterate to a fixed point. Each meta resolves at most once
    * ([[com.vanillasource.eliot.eliotc.monomorphize.domain.MetaRole.Instantiation.combineResolved]]).
    */
  def resolveCombines: CheckIO[Boolean] =
    for {
      s          <- get
      targets     = s.unifier.unresolvedCandidateMetas
      progressed <- targets.foldLeftM(false) { case (acc, (rawId, cs)) =>
                      resolveOneCombine(SemValue.MetaId(rawId), cs).map(_ || acc)
                    }
    } yield progressed

  /** Discharge the deferred "result fits expected" obligations
    * ([[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.pendingUpperBounds]]) after combine resolution has
    * settled each combinable meta to its final solution (a `Combine` join, or its single candidate). The final
    * solution must be definitionally equal to, or `Coerce` into, the expected type; otherwise a mismatch is reported.
    * Run once, after the drain-and-resolve loop, so the join — not the meta's first candidate — is what is checked
    * against a narrower declared type (e.g. `def x: Int[3,5] = pick(integerLiteral[3], integerLiteral[7])`, whose join
    * `Int[3,7]` does not fit `Int[3,5]`).
    */
  def resolveUpperBounds: CheckIO[Unit] =
    for {
      s <- get
      _ <- s.unifier.pendingUpperBounds.traverse_ { case (id, expected, context) =>
             for {
               solution <- force(VMeta(id, Spine.SNil))
               ok       <- coercionExists(solution, expected, context)
               _        <- if (ok) pure(())
                           else modify(st => st.withUnifier(st.unifier.addMismatch(solution, expected, context)))
             } yield ()
           }
    } yield ()

  private def resolveOneCombine(
      id: SemValue.MetaId,
      cs: List[(SemValue, Sourced[String])]
  ): CheckIO[Boolean] =
    for {
      s        <- get
      distinct <- distinctCandidates(cs)
      result   <- distinct match {
                    case None                    => pure(false) // a candidate isn't ground yet — retry next round
                    case Some(ds) if ds.size < 2 => modify(_.recordCombineResolved(id)).as(false)
                    case Some(ds)                =>
                      if (s.unifier.isCombinable(id.value)) combineCandidates(id, ds)
                      else strictReunify(id, ds)
                  }
    } yield result

  /** Quote each candidate to a [[GroundValue]] and keep the first occurrence of each distinct ground (preserving its
    * [[SemValue]] and source context). Returns `None` if any candidate is not yet ground (has unsolved metas).
    */
  private def distinctCandidates(
      cs: List[(SemValue, Sourced[String])]
  ): CheckIO[Option[List[(GroundValue, SemValue, Sourced[String])]]] =
    inspect { s =>
      val quoted = cs.map { case (sem, ctx) => Quoter.quote(0, sem, s.unifier.metaStore).map((_, sem, ctx)) }
      if (quoted.exists(_.isLeft)) None
      else Some(quoted.collect { case Right(t) => t }.distinctBy(_._1))
    }

  private def combineCandidates(
      id: SemValue.MetaId,
      ds: List[(GroundValue, SemValue, Sourced[String])]
  ): CheckIO[Boolean] =
    foldCombine(ds.map(_._1)).flatMap {
      case Some(combinedSem) =>
        for {
          _ <- modify(st => st.withUnifier(st.unifier.copy(metaStore = st.unifier.metaStore.solve(id, combinedSem))))
          _ <- ds.traverse_ { case (_, sem, ctx) => verifyCoercion(sem, combinedSem, ctx) }
          _ <- modify(_.recordCombineResolved(id))
        } yield true
      case None              => strictReunify(id, ds)
    }

  /** Fold a non-empty list of distinct candidate grounds pairwise through the `Combine` ability into a single joined
    * [[SemValue]]. `None` if any pair has no `Combine` instance (or the joined type fails to read back).
    */
  private def foldCombine(grounds: List[GroundValue]): CheckIO[Option[SemValue]] =
    grounds match {
      case Nil          => pure(None)
      case head :: tail =>
        tail.foldLeftM(Option(Evaluator.groundToSem(head))) {
          case (None, _)            => pure(None)
          case (Some(accSem), next) =>
            for {
              s      <- get
              result <- Quoter.quote(0, accSem, s.unifier.metaStore) match {
                          case Left(_)          => pure(None)
                          case Right(accGround) => combinePair(accGround, next)
                        }
            } yield result
        }
    }

  /** Resolve `Combine[g1, g2]` by name and evaluate the resolved instance's associated `Combined` type, binding the
    * instance's leading type parameters to the matched type arguments returned by ability resolution. Returns the
    * joined type's [[SemValue]], or `None` if there is no instance.
    *
    * Only attempted when the two candidates share a head constructor: probing `resolveAbility` for a pair with no
    * implementation triggers the ability machinery's "does not implement" error as a side effect of fact generation
    * (which the caller cannot suppress), so cross-constructor pairs (e.g. `String`/`Int`) skip straight to a strict
    * mismatch. The only `Combine` instance today is the head-homogeneous `Combine[Int[..], Int[..]]`.
    */
  private def combinePair(g1: GroundValue, g2: GroundValue): CheckIO[Option[SemValue]] =
    if (!sameHead(g1, g2))
      pure(None) // Different constructors cannot combine; skip the probe (see [[resolveAbility]] note).
    else
      liftF(resolveAbility(WellKnownTypes.combinedFQN, Seq(g1, g2), Platform.Runtime)).flatMap {
        case None                                  => pure(None)
        case Some((implCombinedFqn, implTypeArgs)) =>
          for {
            orvOpt <- liftF(getFact(OperatorResolvedValue.Key(implCombinedFqn, platform)))
            result <- orvOpt.flatMap(orv => orv.runtime.map((orv, _))) match {
                        case None              => pure(None)
                        case Some((orv, body)) =>
                          for {
                            sigSem   <- evalExpr(orv.typeStack.value.signature, Some(Env.empty))
                            names    <- peelLamNames(sigSem, Seq.empty)
                            env       = names.zip(implTypeArgs).foldLeft(Env.empty) { case (e, (n, g)) =>
                                          e.bind(n, Evaluator.groundToSem(g))
                                        }
                            combined <- evalExpr(body.value, Some(env))
                            forced   <- force(combined)
                          } yield Some(forced)
                      }
          } yield result
      }

  /** Peel the leading `VLam` binder names of an evaluated signature (the impl's bound type parameters, in declaration
    * order), advancing each closure with a rigid placeholder. Mirrors [[peelSigMetas]] but keeps only the names, since
    * the bound values come from ability resolution rather than from unification.
    */
  private def peelLamNames(sem: SemValue, acc: Seq[String]): CheckIO[Seq[String]] =
    force(sem).flatMap {
      case VLam(name, closure) =>
        peelLamNames(closure(VNeutral(NeutralHead.VVar(0, name), Spine.SNil)), acc :+ name)
      case _                   => pure(acc)
    }

  /** Confirm a contributor type coerces (or is definitionally equal) to the joined type; otherwise record the mismatch.
    */
  private def verifyCoercion(actual: SemValue, expected: SemValue, context: Sourced[String]): CheckIO[Unit] =
    coercionExists(actual, expected, context).flatMap {
      case true  => pure(())
      case false => modify(st => st.withUnifier(st.unifier.addMismatch(actual, expected, context)))
    }

  /** Whether `actual` is definitionally equal to, or `Coerce`s into, `expected`. Resolves `Coerce[actual, expected]` by
    * name and evaluates it (the same machinery as check-mode coercion), without re-typing any expression.
    */
  private def coercionExists(actual: SemValue, expected: SemValue, context: Sourced[String]): CheckIO[Boolean] =
    for {
      s      <- get
      result <- s.unifier.tryUnify(actual, expected, context) match {
                  case UnifyResult.Unified(_)       => pure(true)
                  case UnifyResult.Contradiction(_) => resolveCoercion(actual, expected, context)
                }
    } yield result

  /** Re-unify the distinct candidates strictly (first against each subsequent), surfacing the ordinary "Type mismatch."
    * that first-candidate-wins would have produced when no `Combine` join is available.
    */
  private def strictReunify(
      id: SemValue.MetaId,
      ds: List[(GroundValue, SemValue, Sourced[String])]
  ): CheckIO[Boolean] =
    for {
      // Distinct candidates cannot be definitionally equal, so each is a genuine mismatch against the first. Use
      // `addMismatch` (one error per candidate) rather than `unify`, whose spine descent would emit one error per
      // differing type argument (e.g. both `Int` bounds).
      _ <- ds.tail.traverse_ { case (_, sem, ctx) =>
             modify(st => st.withUnifier(st.unifier.addMismatch(ds.head._2, sem, ctx)))
           }
      _ <- modify(_.recordCombineResolved(id))
    } yield true

  private def sameHead(g1: GroundValue, g2: GroundValue): Boolean = (g1, g2) match {
    case (GroundValue.Structure(fqn1, _, _), GroundValue.Structure(fqn2, _, _)) => fqn1 == fqn2
    case _                                                                      => false
  }

  /** Peel leading `VLam` closures (the impl signature's bound type parameters) into fresh metas. Returns the non-lambda
    * head (the value-level function type) and the `name → meta` bindings in peel order.
    */
  private def peelSigMetas(sem: SemValue, acc: Seq[(String, VMeta)]): CheckIO[(SemValue, Seq[(String, VMeta)])] =
    for {
      forced <- force(sem)
      result <- forced match {
                  case VLam(name, closure) =>
                    for {
                      meta <- freshMeta
                      res  <- peelSigMetas(closure(meta), acc :+ (name, meta))
                    } yield res
                  case other               => pure((other, acc))
                }
    } yield result

}
