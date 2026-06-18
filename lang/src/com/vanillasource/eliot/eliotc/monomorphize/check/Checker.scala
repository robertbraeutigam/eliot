package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
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
    * the coercion exists, and the `some` payload is spliced as a real conversion node wrapping the term (see
    * [[buildCoercedExpr]]) — for `Int` that is a `nativeWiden` the backend lowers to an unbox→rebox between the source
    * and target representations. A `none` or stuck result means no coercion, so the original "Type mismatch." is
    * committed.
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
                    liftF(resolveAbility(WellKnownTypes.coerceFQN, Seq(aG, eG))).flatMap {
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
      orvOpt <- liftF(getFact(OperatorResolvedValue.Key(implFqn)))
      result <- orvOpt.flatMap(orv => orv.runtime.map((orv, _))) match {
                  case None              => pure(Option.empty[SemValue])
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
                        case _                                                                 => sem
                      }
                      pure((SemExpression(tpe, SemExpression.ParameterReference(name)), tpe))
                    case None      =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        _      <- ensureBinding(vfqn.value)
        svOpt  <- liftF(getFact(SaturatedValue.Key(vfqn.value)))
        result <- svOpt match {
                    case Some(sv) =>
                      // Read the *saturated* signature, so a callee's parameter-position bare omittable references
                      // (e.g. bare `Int`) present as ordinary leading generic binders that the instantiation machinery
                      // solves from this call's arguments. Signatures reference only their own parameters or top-level
                      // values, so they evaluate under an empty env — outer-session bindings are not in scope.
                      for {
                        sig              <- evalExpr(sv.value.typeStack.value.signature, env = Some(Env.empty))
                        explicitTypeArgs <- typeArgs.traverse(ta => evalExpr(ta.value))
                        appliedSig        = explicitTypeArgs.foldLeft(sig)(Evaluator.applyValue)
                        // W4 (deferred W3 item 1): a calculated-return value referenced as a *complete* value — no
                        // parameters left to apply, so its whole type forced to the `Type` placeholder `saturate`
                        // installed — is resolved from its monomorphized return here, so a no-argument producer used by
                        // name (`def y: Int = x`) works instead of leaking `Type` into a mismatch. The applied case
                        // keeps a `VPi` here (resolved by `applyInferred`); a calculated-return *function* passed
                        // unapplied keeps the placeholder inside its codomain (the higher-order limit, out of scope).
                        calcReturn       <- if (sv.value.calculatedReturn)
                                              resolveCompleteCalculatedReturn(vfqn, explicitTypeArgs, appliedSig)
                                            else pure(Option.empty[SemValue])
                        resultType        = calcReturn.getOrElse(appliedSig)
                      } yield (
                        SemExpression(resultType, SemExpression.ValueReference(vfqn, explicitTypeArgs)),
                        resultType
                      )
                    case None     =>
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
                                   case other    =>
                                     resolveCalculatedReturn(updatedTarget, other).flatMap {
                                       case Some(resolved) => pure(resolved)
                                       case None           => renormalize(other)
                                     }
                                 }
    } yield (
      SemExpression(
        retType,
        SemExpression.FunctionApplication(target.as(updatedTarget), arg.as(argExpr))
      ),
      retType
    )

  /** When a function application has reached the bare omittable return of a *calculated-return* producer (W3 of
    * `docs/implicit-generics-plan.md`), resolve that return from the callee's monomorphized signature rather than the
    * (under-applied) source return left by `saturate`. This is the architectural "back-edge": the callee's concrete
    * type arguments are read off the (instantiated) target value reference, and the caller reads
    * `MonomorphicValue(callee, args).signature`'s deep return type — the body-checked result the callee already
    * produced when monomorphized at those arguments — re-entering it as a [[SemValue]]. It reuses the monomorphization
    * the compiler performs anyway; no symbolic quoting on this path.
    *
    * Returns [[None]] (so the ordinary codomain stands) when this is not a reached calculated return: the target is not
    * a value reference, the return is still a function (an intermediate `VPi` of a partial application), or the callee
    * is not a calculated-return producer. Also [[None]] when the type arguments are not yet ground or no
    * monomorphization exists — the caller then keeps the bare return, which fails the ordinary check downstream rather
    * than being silently mistyped.
    */
  private def resolveCalculatedReturn(
      targetExpr: SemExpression,
      rawReturn: SemValue
  ): CheckIO[Option[SemValue]] =
    force(rawReturn).flatMap {
      // The calculated-return placeholder evaluates to `VType` (saturate replaced the bare omittable return with the
      // kind-correct `Type`). A bare `VType` return here therefore means either a reached calculated return or an
      // ordinary type-level function — confirm with the callee's `calculatedReturn` flag before reading its
      // monomorphized return. An intermediate `VPi` (partial application) or any concrete return falls through to the
      // ordinary codomain, so the SaturatedValue fact is fetched only at the rare `VType`-return application.
      case VType =>
        innermostValueRef(targetExpr) match {
          case Some((fqn, typeArgs)) =>
            liftF(getFact(SaturatedValue.Key(fqn.value))).flatMap {
              case Some(sv) if sv.value.calculatedReturn => readMonomorphicReturn(fqn, typeArgs)
              case _                                     => pure(None)
            }
          case None                  => pure(None)
        }
      case _     => pure(None)
    }

  /** Resolve the return of a *complete* (fully applied) calculated-return value referenced by name — the read-site twin
    * of the [[applyInferred]] back-edge (W4, deferred W3 item 1). The value is complete iff its type forced to the
    * `Type` placeholder `saturate` installed, i.e. no parameter `VPi` remains to apply; then its body-checked return is
    * read from `MonomorphicValue(value, args)` exactly as in the applied path, sharing [[readMonomorphicReturn]]'s
    * recursion guard. Returns [[None]] when a `VPi` still remains (the value is applied later, or passed higher-order
    * with the placeholder buried in its codomain) so the ordinary signature stands.
    */
  private def resolveCompleteCalculatedReturn(
      vfqn: Sourced[ValueFQN],
      typeArgs: Seq[SemValue],
      appliedSig: SemValue
  ): CheckIO[Option[SemValue]] =
    force(appliedSig).flatMap {
      case VType => readMonomorphicReturn(vfqn, typeArgs)
      case _     => pure(None)
    }

  /** The innermost [[SemExpression.ValueReference]] of a (possibly curried) application target, with its accumulated
    * type arguments — the callee whose calculated return is being resolved.
    */
  private def innermostValueRef(expr: SemExpression): Option[(Sourced[ValueFQN], Seq[SemValue])] =
    expr.expression match {
      case SemExpression.ValueReference(fqn, typeArgs)  => Some((fqn, typeArgs))
      case SemExpression.FunctionApplication(target, _) => innermostValueRef(target.value)
      case _                                            => None
    }

  /** Read the callee's body-checked return at the concrete type arguments: quote the (instantiation-meta) type args to
    * ground and read `MonomorphicValue(callee, args).signature`'s deep return type as a [[SemValue]]. [[None]] when the
    * args are not yet ground (left as a bare return for the ordinary check to reject) or no monomorphization exists.
    */
  private def readMonomorphicReturn(
      fqn: Sourced[ValueFQN],
      typeArgs: Seq[SemValue]
  ): CheckIO[Option[SemValue]] =
    for {
      s          <- get
      groundArgsE = typeArgs.toList.traverse(a => Quoter.quote(0, a, s.unifier.metaStore))
      result     <- groundArgsE match {
                      case Left(_)           => reportUngroundCalculatedReturn(fqn)
                      case Right(groundArgs) => readMonomorphicReturnGround(fqn, groundArgs)
                    }
    } yield result

  /** W4 (Limit 3 / deferred W3 item 2): a calculated return is read off `MonomorphicValue(callee, args)`, so the
    * callee's type arguments must be ground at the call. They are not when an argument's bounds come from a branch join
    * (a `Combine`) that is resolved only later, in the drain loop — `double(pick(a, b))` instantiates `double`'s bounds
    * from `pick`'s combinable result, which is deferred. Reading the return eagerly here would leave the bare `Type`
    * placeholder, which then leaks into a confusing `Coerce` mismatch downstream. Report a specific, actionable error
    * instead. (Resolving such a call by postponing the calculation past the join — making it *compile* — is a
    * completeness improvement deferred to W5; it requires reordering against the combinable-meta machinery.)
    */
  private def reportUngroundCalculatedReturn(fqn: Sourced[ValueFQN]): CheckIO[Option[SemValue]] =
    liftF(
      compilerError(
        fqn.as(
          s"Cannot calculate the return type of '${fqn.value.name.name}' here: its argument bounds are not determined at this call site."
        ),
        Seq(
          "This happens when an argument's bounds come from a branch join (a `Combine`) not yet resolved when the call is checked.",
          "Annotate the argument's type, or give the value an explicit return type."
        )
      ) >> abort[Option[SemValue]]
    )

  /** Read the callee's monomorphized return at ground type arguments, first guarding against a recursive
    * calculated-return chain (Limit 1 of `docs/implicit-generics-plan.md`). If the callee's FQN is already an ancestor
    * of the fact being checked now, requesting `MonomorphicValue(callee, args)` would re-enter an in-progress
    * computation and dead-lock the fact cache — and, more fundamentally, the callee's return depends (directly,
    * mutually, or through a value-dependent bound) on itself, which monomorphization-by-type cannot ground. A
    * non-recursive program has an acyclic producer call graph, so a repeated FQN on the active chain is exactly the
    * recursion signal; report it as a specific error rather than blocking forever or defaulting to `Type`.
    */
  private def readMonomorphicReturnGround(
      fqn: Sourced[ValueFQN],
      groundArgs: Seq[GroundValue]
  ): CheckIO[Option[SemValue]] =
    liftF(activeFactKeys).flatMap { active =>
      val recursing = active.exists {
        case MonomorphicValue.Key(vfqn, _) => vfqn == fqn.value
        case _                             => false
      }
      if (recursing) liftF(reportRecursiveCalculatedReturn(fqn) >> abort[Option[SemValue]])
      else
        liftF(getFact(MonomorphicValue.Key(fqn.value, groundArgs)))
          .map(_.map(mv => Evaluator.groundToSem(mv.signature.deepReturnType)))
    }

  private def reportRecursiveCalculatedReturn(fqn: Sourced[ValueFQN]): CompilerIO[Unit] =
    compilerError(
      fqn.as(s"Cannot calculate the return type of recursive value '${fqn.value.name.name}'."),
      Seq(
        "Its result type depends on itself — directly, mutually, or through a value-dependent bound — which " +
          "monomorphization cannot ground.",
        "Write an explicit return type."
      )
    )

  /** Replace the return position of a calculated-return signature with a fresh metavariable, returning the rewritten
    * signature and the meta's id (W3 callee side). The signature is a chain of value-parameter `VPi` arrows ending in
    * the bare omittable return left by `saturate`; the meta stands in for that return so that *checking the body*
    * against this signature solves it (by ordinary unification) to the body's inferred type. A no-parameter producer
    * (`def x: Int = 5`) has no arrows, so the signature *is* the return and is replaced directly. The descent reads a
    * snapshot of the metastore to tell a `VPi` arrow from the return; that is stable, because the arrows come from the
    * (meta-independent) `Function` native, not from any meta solution.
    */
  private[check] def installReturnMeta(sig: SemValue): CheckIO[(SemValue, VMeta)] =
    for {
      meta <- freshMeta
      s    <- get
    } yield (substituteReturn(sig, meta, s.unifier.metaStore), meta)

  private def substituteReturn(sig: SemValue, meta: SemValue, metaStore: MetaStore): SemValue =
    Evaluator.force(sig, metaStore) match {
      case VPi(domain, codomain) => VPi(domain, arg => substituteReturn(codomain(arg), meta, metaStore))
      case _                     => meta
    }

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
