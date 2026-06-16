package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Bidirectional type checker for the NbE pipeline. All state is threaded via the CheckIO state monad.
  *
  *   - `check(tm, expected)` checks a term against a known type.
  *   - `infer(tm)` infers a term's type.
  *
  * The checker produces [[SemExpression]]s with [[SemValue]] in every type slot. All ground-type conversion is deferred
  * to a post-drain pass in [[TypeStackLoop]], using [[com.vanillasource.eliot.eliotc.monomorphize.eval. Quoter]]. This
  * avoids any silent "default to Type" behaviour for unsolved metas — they surface as explicit errors at quoting time.
  */
class Checker(
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]]
) {

  /** Ensure a NativeBinding is in the cache, fetching it via CompilerIO if needed.
    *
    * References to abstract associated ability types (`type X` inside `ability ...`, no body) are rewritten to a fresh
    * [[VMeta]] on first access and cached in that form. The meta is solved post-drain by unifying against the concrete
    * impl's corresponding associated-type value. The cache provides per-(fqn, check-session) dedup automatically —
    * subsequent lookups return the same cached meta.
    */
  private def ensureBinding(vfqn: ValueFQN): CheckIO[Option[SemValue]] =
    for {
      cached <- inspect(_.bindingCache.get(vfqn))
      result <- cached match {
                  case Some(value) => pure(value)
                  case None        =>
                    for {
                      opt      <- liftF(fetchBinding(vfqn))
                      replaced <- opt match {
                                    case Some(VTopDef(fqn, None, Spine.SNil)) if ValueFQN.isAbstractAbilityType(fqn) =>
                                      for {
                                        meta <- freshMeta
                                        _    <- modify(_.recordAbstractTypeMeta(vfqn, meta.id))
                                      } yield Some(meta: SemValue)
                                    case other                                                                       =>
                                      pure(other)
                                  }
                      _        <- modify(_.cacheBinding(vfqn, replaced))
                    } yield replaced
                }
    } yield result

  /** Evaluate an ORE expression against an env (defaulting to the current state's env). Prefetches every reachable
    * binding into [[CheckState.bindingCache]] first — including rewriting abstract associated-ability-types to fresh
    * metas via [[ensureBinding]] — so that the pure [[Evaluator]] has everything it needs.
    */
  def evalExpr(tm: OperatorResolvedExpression, env: Option[Env] = None): CheckIO[SemValue] =
    for {
      _ <- prefetchBindings(tm)
      s <- get
    } yield s.makeEvaluator.eval(env.getOrElse(s.env), tm)

  /** Force a SemValue through the current meta store. */
  private[check] def force(v: SemValue): CheckIO[SemValue] =
    inspect(s => Evaluator.force(v, s.unifier.metaStore))

  /** Deeply normalise a SemValue, re-firing stuck native applications (e.g. the dependent-bounds `add` in
    * `Int[add(LMin,RMin), …]`) whose bound arguments have since been solved. Uses the binding cache as the native
    * lookup. See [[Evaluator.renormalize]].
    */
  private[check] def renormalize(v: SemValue): CheckIO[SemValue] =
    inspect(s => Evaluator.renormalize(v, s.unifier.metaStore, fqn => s.bindingCache.getOrElse(fqn, None)))

  /** Unify two semantic values, updating the unifier in the state. */
  private def doUnify(l: SemValue, r: SemValue, context: Sourced[String]): CheckIO[Unit] =
    modify(s => s.withUnifier(s.unifier.unify(l, r, context)))

  /** Allocate a fresh metavariable. */
  private[check] def freshMeta: CheckIO[VMeta] =
    for {
      s                   <- get
      (metaId, freshStore) = s.unifier.metaStore.fresh
      _                   <- modify(_.withUnifier(s.unifier.copy(metaStore = freshStore)))
    } yield VMeta(metaId, Spine.SNil)

  /** Check a term against a known expected type. */
  def check(
      tm: Sourced[OperatorResolvedExpression],
      expected: SemValue
  ): CheckIO[SemExpression] =
    for {
      forcedExpected <- force(expected)
      result         <- tm.value match {
                          // FunctionLiteral against a known VPi — unify domain, bind param, check body against codomain.
                          // Works for both annotated (unify annotated paramType with domain) and unannotated (use domain
                          // as paramType). Attribution falls to the body on codomain mismatches.
                          case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeStack, body)
                              if forcedExpected.isInstanceOf[VPi] =>
                            val VPi(domain, codomain) = forcedExpected: @unchecked
                            for {
                              paramType <- paramTypeStack match {
                                             case Some(ts) =>
                                               for {
                                                 pt <- evalExpr(ts.value.signature)
                                                 _  <- doUnify(pt, domain, paramName.as("Type mismatch."))
                                               } yield pt
                                             case None     => pure(domain)
                                           }
                              _         <- modify(_.bind(paramName.value, paramType))
                              bodyExpr  <- check(body, codomain(paramType))
                            } yield SemExpression(
                              forcedExpected,
                              SemExpression.FunctionLiteral(paramName, paramType, body.as(bodyExpr))
                            )

                          // Unannotated FunctionLiteral against non-VPi expected — cannot infer param type.
                          case OperatorResolvedExpression.FunctionLiteral(_, None, _) =>
                            liftF(compilerError(tm.as("Cannot infer type of unannotated lambda.")) >> abort)

                          case _ =>
                            for {
                              (expr, inferred) <- infer(tm)
                              // Defer only when checking, against a *concrete* expected type, a combinable-meta result
                              // that actually accumulated argument contributions (so it could be a `Combine` join). Two
                              // guards matter: (1) if `expected` is itself a (flowing) meta this is an intermediate
                              // contribution — e.g. `id(i)` as an argument — and must unify now to propagate through the
                              // chain; (2) a result meta with *no* candidates (e.g. an ability method's `B` in
                              // `convert(x): B`, or any fresh result) has no join to wait for and must unify now so the
                              // value/ability resolution that depends on its solution can proceed.
                              forcedExp        <- force(expected)
                              combinableMeta   <- (inferred, forcedExp) match {
                                                    case (VMeta(id, Spine.SNil), exp) if !exp.isInstanceOf[VMeta] =>
                                                      inspect(s =>
                                                        Option.when(
                                                          s.unifier.combinable.contains(id.value) &&
                                                            s.unifier.candidates.get(id.value).exists(_.nonEmpty)
                                                        )(id)
                                                      )
                                                    case _                                                        => pure(None)
                                                  }
                              checked          <- combinableMeta match {
                                                    // The term's type is a bare combinable meta — the result of a
                                                    // polymorphic call whose result type is a type parameter. Its final
                                                    // solution (possibly a `Combine` join) is unknown until drain, so
                                                    // defer the check against `expected` rather than committing it
                                                    // against the meta's first candidate (which would unsoundly accept a
                                                    // join that overflows a narrower `expected`). See resolveUpperBounds.
                                                    case Some(id) =>
                                                      modify(_.recordUpperBound(id, expected, tm.as("Type mismatch.")))
                                                        .as(expr)
                                                    case None     =>
                                                      for {
                                                        (updatedExpr, instantiated) <-
                                                          instantiatePolymorphic(expr, inferred)
                                                        c                           <- unifyOrCoerce(tm, updatedExpr, instantiated, expected)
                                                      } yield c
                                                  }
                            } yield checked
                        }
    } yield result

  /** Resolve a term against an expected type. First attempt pure definitional equality (the unifier). If that fails,
    * attempt a check-mode `Coerce` insertion: where the inferred type is used where a different expected type built
    * from the same constructor is wanted (e.g. `Int[3,3]` where `Int[0,10]` is expected), resolve the user-defined
    * `Coerce` ability by name and evaluate its `coerce` through the one NbE evaluator. A `some payload` result means
    * the coercion exists; the term is re-typed at `expected` (the widening payload `nativeWiden` is the identity on the
    * current Long-only backend, so no runtime rewrite is needed; a real read-back lands with range-based width
    * selection). A `none` or stuck result means no coercion,
    * so the original "Type mismatch." is committed.
    *
    * `unify` itself stays pure definitional equality — the directional widening lives here, in check mode only.
    */
  private def unifyOrCoerce(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue
  ): CheckIO[SemExpression] =
    for {
      s      <- get
      trial   = s.unifier.unify(actual, expected, tm.as("Type mismatch."))
      result <- if (trial.errors.size == s.unifier.errors.size)
                  modify(_.withUnifier(trial)).as(expr)
                else
                  tryCoerce(tm, expr, actual, expected).flatMap {
                    case Some(coerced) => pure(coerced)
                    // No coercion: report a single mismatch (carrying Expected/Actual) at the term rather than
                    // committing `trial`, whose spine descent can yield one error per mismatched type argument (e.g.
                    // both `Int` bounds).
                    case None          =>
                      modify(st => st.withUnifier(st.unifier.addMismatch(actual, expected, tm.as("Type mismatch."))))
                        .as(expr)
                  }
    } yield result

  /** Try to resolve and apply a `Coerce[actual, expected]` instance. Returns the re-typed expression on success (the
    * coercion's `coerce` evaluated to `some`), or `None` when no coercion applies (no instance, a `none` result, or
    * bounds too abstract to decide). Only fires for leaf positions (argument / binding / return); coercing inside a
    * constructor (e.g. `List[Int[0,5]] → List[Int[0,10]]`) needs variance reasoning and is not handled here.
    */
  private def tryCoerce(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue
  ): CheckIO[Option[SemExpression]] =
    resolveCoercion(actual, expected, tm.as("Type mismatch.")).map { holds =>
      // On success re-type the term at `expected`. The widening payload `nativeWiden` is the identity on the current
      // Long-only backend, so no runtime rewrite is needed; a real read-back lands with range-based width selection.
      if (holds) Some(expr.copy(expressionType = expected)) else None
    }

  /** Resolve `Coerce[actual, expected]` by name and report whether the coercion exists (the resolved `coerce` evaluates
    * to `some`). This is the shared core of the check-mode insertion ([[tryCoerce]]) and the `Combine` contributor
    * check ([[coercionExists]]) — both need "does this coercion hold?" but differ in what they do with the answer. It
    * does **not** attempt definitional equality first (callers decide whether to); it returns `false` when the bounds
    * are abstract/unsolved (cannot quote to ground), when no instance resolves, or when the instance yields `none`.
    */
  private def resolveCoercion(actual: SemValue, expected: SemValue, context: Sourced[String]): CheckIO[Boolean] =
    for {
      s      <- get
      grounds = for {
                  a <- Quoter.quote(0, actual, s.unifier.metaStore)
                  e <- Quoter.quote(0, expected, s.unifier.metaStore)
                } yield (a, e)
      result <- grounds match {
                  case Left(_)         => pure(false) // abstract / unsolved bounds — cannot prove a coercion
                  case Right((aG, eG)) =>
                    liftF(resolveAbility(WellKnownTypes.coerceFQN, Seq(aG, eG))).flatMap {
                      case None               => pure(false)
                      case Some((implFqn, _)) => coercionHolds(implFqn, actual, expected, context)
                    }
                }
    } yield result

  /** Evaluate the resolved `coerce` implementation against the concrete bounds and report whether it yields `some` (the
    * coercion exists). Invoked through [[resolveCoercion]] once an instance has been resolved by name.
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
  private def coercionHolds(
      implFqn: ValueFQN,
      actual: SemValue,
      expected: SemValue,
      context: Sourced[String]
  ): CheckIO[Boolean] =
    for {
      orvOpt <- liftF(getFact(OperatorResolvedValue.Key(implFqn)))
      result <- orvOpt.flatMap(orv => orv.runtime.map((orv, _))) match {
                  case None              => pure(false)
                  case Some((orv, body)) =>
                    for {
                      sigSem        <- evalExpr(orv.typeStack.value.signature, env = Some(Env.empty))
                      (mono, binds) <- peelSigMetas(sigSem, Seq.empty)
                      optionExpected =
                        Evaluator.applyValue(VTopDef(WellKnownTypes.optionFQN, None, Spine.SNil), expected)
                      _             <- doUnify(mono, VPi(actual, _ => optionExpected), context)
                      _             <- modify(st => st.withUnifier(st.unifier.drain()))
                      concreteEnv   <- binds.foldLeftM(Env.empty) { case (env, (name, meta)) =>
                                         force(meta).map(env.bind(name, _))
                                       }
                      bodyVal       <- evalExpr(body.value, env = Some(concreteEnv))
                      applied        = Evaluator.applyValue(bodyVal, VNeutral(NeutralHead.VVar(0, "$coerceArg"), Spine.SNil))
                      forced        <- force(applied)
                    } yield forced match {
                      case VTopDef(fqn, _, _) if fqn == WellKnownTypes.someFQN => true
                      case _                                                   => false
                    }
                }
    } yield result

  /** Resolve every combinable metavariable that accumulated more than one distinct candidate type
    * ([[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.candidates]]). For a still-combinable (covariant)
    * meta the candidates are folded pairwise through the `Combine` ability into their join `R`; the meta is solved to
    * `R` and each contributor is verified to `Coerce` into `R` (always true for `Int` by construction). When no
    * `Combine` instance applies — or the meta was tainted by a contravariant use — the candidates are re-unified
    * strictly, which surfaces the ordinary "Type mismatch." that first-candidate-wins would have produced. Returns
    * whether any meta was newly resolved, so the caller can iterate to a fixed point. Each meta resolves at most once
    * ([[CheckState.combineResolved]]).
    */
  def resolveCombines: CheckIO[Boolean] =
    for {
      s          <- get
      targets     = s.unifier.candidates.toList.filterNot { case (rawId, _) => s.combineResolved.contains(rawId) }
      progressed <- targets.foldLeftM(false) { case (acc, (rawId, cs)) =>
                      resolveOneCombine(SemValue.MetaId(rawId), cs).map(_ || acc)
                    }
    } yield progressed

  /** Discharge the deferred "result fits expected" obligations ([[CheckState.pendingUpperBounds]]) after combine
    * resolution has settled each combinable meta to its final solution (a `Combine` join, or its single candidate). The
    * final solution must be definitionally equal to, or `Coerce` into, the expected type; otherwise a mismatch is
    * reported. Run once, after the drain-and-resolve loop, so the join — not the meta's first candidate — is what is
    * checked against a narrower declared type (e.g. `def x: Int[3,5] = pick(integerLiteral[3], integerLiteral[7])`,
    * whose join `Int[3,7]` does not fit `Int[3,5]`).
    */
  def resolveUpperBounds: CheckIO[Unit] =
    for {
      s <- get
      _ <- s.pendingUpperBounds.traverse_ { case (id, expected, context) =>
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
                      if (s.unifier.combinable.contains(id.value)) combineCandidates(id, ds)
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
      liftF(resolveAbility(WellKnownTypes.combinedFQN, Seq(g1, g2))).flatMap {
        case None                                  => pure(None)
        case Some((implCombinedFqn, implTypeArgs)) =>
          for {
            orvOpt <- liftF(getFact(OperatorResolvedValue.Key(implCombinedFqn)))
            result <- orvOpt.flatMap(orv => orv.runtime.map((orv, _))) match {
                        case None              => pure(None)
                        case Some((orv, body)) =>
                          for {
                            sigSem   <- evalExpr(orv.typeStack.value.signature, env = Some(Env.empty))
                            names    <- peelLamNames(sigSem, Seq.empty)
                            env       = names.zip(implTypeArgs).foldLeft(Env.empty) { case (e, (n, g)) =>
                                          e.bind(n, Evaluator.groundToSem(g))
                                        }
                            combined <- evalExpr(body.value, env = Some(env))
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
      trial   = s.unifier.unify(actual, expected, context)
      result <- if (trial.errors.size == s.unifier.errors.size) pure(true)
                else resolveCoercion(actual, expected, context)
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

  /** Peel leading VLam closures by substituting fresh metas; return the non-VLam head together with the fresh metas in
    * order.
    *
    * @param bindInEnv
    *   Whether to bind each peeled parameter name to its fresh meta in the env. `false` (the default) is required for
    *   polytype instantiation inside the checker, so the callee's type-parameter names don't shadow any caller-scope
    *   parameter with the same name. `true` is used only by the top-level type-stack walk, where leftover type
    *   parameters should become in-scope names.
    */
  private[check] def peelLams(
      sem: SemValue,
      bindInEnv: Boolean = false
  ): CheckIO[(SemValue, Seq[SemValue])] = {
    def loop(s: SemValue, acc: Seq[SemValue]): CheckIO[(SemValue, Seq[SemValue])] =
      for {
        forced <- force(s)
        result <- forced match {
                    case VLam(name, closure) =>
                      for {
                        meta <- freshMeta
                        // Implicit type-parameter instantiation metas sit in covariant positions (a match result, a
                        // result-position type param), so they are eligible for `Combine`-based multi-candidate
                        // resolution. The unifier taints any that later flow into a contravariant (VPi domain) position.
                        _    <- modify(s => s.withUnifier(s.unifier.markCombinable(meta.id)))
                        _    <- if (bindInEnv) modify(_.bind(name, meta)) else pure(())
                        rest <- loop(closure(meta), acc :+ meta)
                      } yield rest
                    case other               => pure((other, acc))
                  }
      } yield result
    loop(sem, Seq.empty)
  }

  /** Infer the type of a term. */
  def infer(
      tm: Sourced[OperatorResolvedExpression]
  ): CheckIO[(SemExpression, SemValue)] = tm.value match {
    case OperatorResolvedExpression.IntegerLiteral(value) =>
      // Use the same VTopDef shape that DataTypeNativesProcessor binds for BigInteger, so the unifier sees a
      // single canonical form for this type rather than a VConst(Structure) vs VTopDef mismatch.
      val tpe = VTopDef(WellKnownTypes.bigIntFQN, None, Spine.SNil)
      pure((SemExpression(tpe, SemExpression.IntegerLiteral(value)), tpe))

    case OperatorResolvedExpression.StringLiteral(value) =>
      val tpe = VTopDef(WellKnownTypes.stringFQN, None, Spine.SNil)
      pure((SemExpression(tpe, SemExpression.StringLiteral(value)), tpe))

    case OperatorResolvedExpression.ParameterReference(name) =>
      for {
        state  <- get
        result <- state.env.lookupByName(name.value) match {
                    case Some(sem) =>
                      // A runtime value parameter's env binding *is* its type, so it serves as the inferred type
                      // directly. An erased type-stack value parameter's binding is its concrete *value* instead (e.g.
                      // `A: Person` bound to `Person("Alice", …)`); referenced in value position its type is the value's
                      // type, recovered here. (Type-position references go through evaluation, never this path, so they
                      // still see the concrete value.)
                      val tpe = sem match {
                        case VConst(ground) if state.typeStackValueParams.contains(name.value) =>
                          Evaluator.groundToSem(ground.valueType)
                        case _                                                                  => sem
                      }
                      pure((SemExpression(tpe, SemExpression.ParameterReference(name)), tpe))
                    case None      =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        _      <- ensureBinding(vfqn.value)
        orvOpt <- liftF(getFact(OperatorResolvedValue.Key(vfqn.value)))
        result <- orvOpt match {
                    case Some(orv) =>
                      // Signatures reference only their own parameters or top-level values, so they evaluate under an
                      // empty env — outer-session bindings are not in scope.
                      for {
                        sig              <- evalExpr(orv.typeStack.value.signature, env = Some(Env.empty))
                        explicitTypeArgs <- typeArgs.traverse(ta => evalExpr(ta.value))
                      } yield {
                        val appliedSig = explicitTypeArgs.foldLeft(sig)(Evaluator.applyValue)
                        (
                          SemExpression(appliedSig, SemExpression.ValueReference(vfqn, explicitTypeArgs)),
                          appliedSig
                        )
                      }
                    case None      =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        (targetExpr, targetType) <- infer(target)
        result                   <- applyInferred(target, targetExpr, targetType, arg)
      } yield result

    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
      for {
        paramType            <- evalExpr(paramTypeStack.value.signature)
        _                    <- modify(_.bind(paramName.value, paramType))
        (bodyExpr, bodyType) <- infer(body)
        tpe                   = VPi(paramType, _ => bodyType)
      } yield (
        SemExpression(
          tpe,
          SemExpression.FunctionLiteral(paramName, paramType, body.as(bodyExpr))
        ),
        tpe
      )

    case OperatorResolvedExpression.FunctionLiteral(_, None, _) =>
      liftF(compilerError(tm.as("Cannot infer type of unannotated lambda.")) >> abort)
  }

  /** Handle function application: peel any polytype (`VLam`) layers with fresh metas, then apply one argument to the
    * resulting monotype. If the monotype isn't already `VPi`, it gets unified against a fresh one. The implicit metas
    * introduced by peeling are baked into the target reference.
    */
  private def applyInferred(
      target: Sourced[OperatorResolvedExpression],
      targetExpr: SemExpression,
      targetType: SemValue,
      arg: Sourced[OperatorResolvedExpression]
  ): CheckIO[(SemExpression, SemValue)] =
    for {
      (updatedTarget, peeled) <- instantiatePolymorphic(targetExpr, targetType)
      vpi                     <- peeled match {
                                   case p: VPi => pure(p)
                                   case _      =>
                                     for {
                                       domMeta <- freshMeta
                                       codMeta <- freshMeta
                                       p        = VPi(domMeta, _ => codMeta)
                                       _       <- doUnify(peeled, p, target.as("Not a function."))
                                     } yield p
                                 }
      argExpr                 <- check(arg, vpi.domain)
      argSem                  <- evalExpr(arg.value)
      // The codomain may embed a native applied to the target's instantiation metas — e.g. `+`'s result type
      // `Int[add(LMin,RMin), …]`. Those bounds are solved by the argument checks just above, so renormalise the
      // codomain now to re-fire the natives (`add(3,4) ⤳ 7`) before the type reaches unification or quoting. A *bare*
      // metavariable result (a result-position type parameter, e.g. `pick[A](a,b): A`) is left untouched: forcing it to
      // its provisional first candidate would destroy the combinable-meta signal the `check` fallback uses to defer the
      // `Combine` join (see `check` / resolveUpperBounds).
      rawRetType               = vpi.codomain(argSem)
      retType                 <- rawRetType match {
                                   case _: VMeta => pure(rawRetType)
                                   case other    => renormalize(other)
                                 }
    } yield (
      SemExpression(
        retType,
        SemExpression.FunctionApplication(target.as(updatedTarget), arg.as(argExpr))
      ),
      retType
    )

  /** Peel leading `VLam` closures from an inferred type with fresh metas, baking the metas as implicit type arguments
    * onto the expression's [[SemExpression.ValueReference]] and updating its `expressionType`. Returns the updated
    * expression paired with the peeled (monotype) type. Used both by the generic `check` fallback and by
    * [[applyInferred]] — any polytype introduced by referencing a generic value gets instantiated at exactly one place.
    */
  private def instantiatePolymorphic(
      expr: SemExpression,
      tpe: SemValue
  ): CheckIO[(SemExpression, SemValue)] =
    for {
      (peeled, implicitMetas) <- peelLams(tpe)
      updated                 <- appendTypeArgs(expr, implicitMetas)
    } yield (updated.copy(expressionType = peeled), peeled)

  /** Append implicit-meta type args to a [[SemExpression.ValueReference]] expression. Only a value reference can
    * inherit a polytype (since polymorphism lives on named signatures), so no other shape should ever arrive here with
    * non-empty `extraArgs`. Hitting that branch indicates a compiler bug.
    */
  private def appendTypeArgs(expr: SemExpression, extraArgs: Seq[SemValue]): CheckIO[SemExpression] =
    if (extraArgs.isEmpty) pure(expr)
    else
      expr.expression match {
        case ref: SemExpression.ValueReference =>
          val updatedArgs = ref.typeArguments ++ extraArgs
          pure(expr.copy(expression = ref.copy(typeArguments = updatedArgs)))
        case other                             =>
          throw new IllegalStateException(
            s"Polytype instantiation produced implicit type arguments for a non-reference expression: $other"
          )
      }

  /** Prefetch-only traversal: walks an ORE and calls [[ensureBinding]] at every ValueReference, discarding any
    * resulting SemValue. Used for subtrees whose actual evaluation is deferred to a pure [[Evaluator]] invocation
    * inside a [[VLam]] closure — the closure must find every reachable binding already in the cache.
    */
  private def prefetchBindings(ore: OperatorResolvedExpression): CheckIO[Unit] =
    OperatorResolvedExpression.foldValueReferences[CheckIO, Unit](ore, ()) { (_, vfqn) =>
      ensureBinding(vfqn.value).void
    }

}
