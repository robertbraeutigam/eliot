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
  *     widening). This is not equality-shaped, which is why it was factored out of the equality core.
  *
  * The `Combine` branch-join (least-upper-bound of covariant multi-candidate metavariables) and its deferred
  * "result fits expected" upper-bound obligations — the other non-equality features this module once hosted — were
  * removed with the refinement channel's flag day (`docs/bounds-as-refinements.md` Step 7b): once `Int` lost its type
  * parameters, `Int == Int` makes the branch-join a no-op (arms are the same type), so the checker no longer joins.
  * What remains is the single `Coerce` insertion entry point, [[unifyOrCoerce]] — retired next at Step 7a.
  *
  * Operates over [[com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO]], reading and writing the shared
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.CheckState]] through `get`/`modify`/`inspect`.
  *
  * @param resolveAbility
  *   Resolve an ability instance by name and ground type arguments (the `Coerce` instance lookup).
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
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
    evalExpr: (OperatorResolvedExpression, Option[Env]) => CheckIO[SemValue],
    force: SemValue => CheckIO[SemValue],
    freshMeta: CheckIO[VMeta],
    doUnify: (SemValue, SemValue, Sourced[String]) => CheckIO[Unit],
    platform: Platform
) {

  /** Resolve a term against an expected type. First attempt pure definitional equality (the unifier). If that fails,
    * attempt a check-mode `Coerce` insertion: where the inferred type is used where a different expected type built
    * from the same constructor is wanted (e.g. `Int[3,3]` where `Int[0,10]` is expected), resolve the user-defined
    * `Coerce` ability by name. A guarded instance *applies* precisely when the coercion is valid, so a resolved
    * instance is the existence proof; its total `coerce` body, evaluated through the one NbE evaluator, is spliced as a
    * real conversion node wrapping the term (see [[buildCoercedExpr]]) — for `Int` that is a `nativeWiden` the backend
    * lowers to an unbox→rebox between the source and target representations. No applicable instance (or bounds too
    * abstract to decide) means no coercion, so the original "Type mismatch." is committed.
    *
    * `unify` itself stays pure definitional equality — the directional widening lives here, in check mode only.
    */
  def unifyOrCoerce(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue
  ): CheckIO[SemExpression] =
    tryUnifyOrCoerce(tm, expr, actual, expected).flatMap {
      case Some(resolved) => pure(resolved)
      // No coercion: report a single mismatch (carrying Expected/Actual) at the term rather than
      // committing the failed unification, whose spine descent can yield one error per mismatched
      // type argument (e.g. both `Int` bounds).
      case None           =>
        modify(st => st.withUnifier(st.unifier.addMismatch(actual, expected, tm.as("Type mismatch."))))
          .as(expr)
    }

  /** The non-committing equality-and-`Coerce` pair: definitional equality, else a `Coerce` insertion. Returns the
    * resolved expression, or [[None]] with *no error committed* when neither applies — the caller decides what
    * follows. Its sole caller is [[unifyOrCoerce]] (the `let`-level resolution), which commits the mismatch; the
    * checker's slot ladder does *not* use this pairing — it interleaves the effect-lift arms between equality and
    * [[tryCoerce]] (lift-arms-before-`Coerce`), composing the same arms itself.
    */
  def tryUnifyOrCoerce(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue
  ): CheckIO[Option[SemExpression]] =
    for {
      s      <- get
      result <- s.unifier.tryUnify(actual, expected, tm.as("Type mismatch.")) match {
                  case UnifyResult.Unified(u)       => modify(_.withUnifier(u)).as(Option(expr))
                  case UnifyResult.Contradiction(_) => tryCoerce(tm, expr, actual, expected)
                }
    } yield result

  /** Try to resolve and apply a `Coerce[actual, expected]` instance. Returns the coerced expression on success (the
    * resolved `coerce`'s body spliced around the term — see [[buildCoercedExpr]]), or `None` when no coercion
    * applies (no applicable instance, or bounds too abstract to decide). Only fires for leaf positions (argument
    * / binding / return); coercing inside a constructor (e.g. `List[Int[0,5]] → List[Int[0,10]]`) needs variance
    * reasoning and is not handled here.
    *
    * Exposed to the checker so the effect-lift arms can be consulted *between* definitional equality and this probe:
    * a pure-into-carrier or carrier-into-pure shape is the lift arms' decision, so it must be theirs before a `Coerce`
    * lookup gets a say. (The probe itself is silent — the ability machinery registers a resolution *outcome*, and only
    * a demanding use site reports a failure; a probe merely reads "no applicable instance" as a decline.) The lift
    * arms' guards are disjoint from every coercible shape (an `Int` range is neither carrier-headed nor an ambient
    * carrier), so current widening behaviour is untouched.
    */
  private[monomorphize] def tryCoerce(
      tm: Sourced[?],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue
  ): CheckIO[Option[SemExpression]] =
    resolveCoercionPayload(actual, expected, tm.as("Type mismatch.")).flatMap {
      case None          => pure(None)
      case Some(payload) => buildCoercedExpr(tm, expr, actual, expected, payload).map(Some(_))
    }

  /** Build the coerced expression from the resolved `Coerce` instance's conversion payload (the total `coerce` body
    * evaluated with its argument left as the reserved `NeutralHead.Marker.Coerce` marker). This is the principled Phase-5
    * splice: rather
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
      tm: Sourced[?],
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
    case Some(VNeutral(NeutralHead.Reserved(NeutralHead.Marker.Coerce), Spine.SNil)) => true
    case _                                                                           => false
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

  /** Resolve `Coerce[actual, expected]` by name and return the resolved instance's conversion payload (the value the
    * coercion yields, with the `coerce` argument left as the `NeutralHead.Marker.Coerce` marker), or [[None]] when no
    * coercion applies (abstract/unsolved bounds, or no applicable instance — a guarded instance whose guard declines).
    * The shared core of [[tryCoerce]] (which splices the payload) and [[resolveCoercion]] (which only needs existence).
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

  /** Evaluate the resolved `coerce` implementation against the concrete bounds and return its conversion payload — the
    * total `coerce` body evaluated with its value argument left as the `NeutralHead.Marker.Coerce` marker neutral, so the
    * payload carries that marker wherever the runtime value flows and [[buildCoercedExpr]] splices the actual expression
    * in its place. The guard already decided applicability (a non-fitting range resolved no instance), so a resolved
    * instance always yields a real conversion — there is no `some`/`none` to interpret.
    *
    * The impl's generic bound parameters are not lambdas in its runtime body (only value parameters are — see
    * `CoreExpressionConverter.buildCurriedBody`); they are free names resolved through the env. So:
    *   - peel the impl signature's type-parameter `VLam`s into fresh metas (recording each `name → meta`);
    *   - unify the resulting value-level function type against `VPi(actual, _ => expected)` and drain, which solves those
    *     metas to the concrete bounds;
    *   - rebuild the body env binding each name to the **forced (concrete)** meta value — not the meta itself: the NbE
    *     evaluator has no metastore, so a native applied to an unforced `VMeta` would go stuck during evaluation.
    *
    * Evaluating the runtime body in that concrete env and forcing it yields the conversion (`nativeWiden(marker)`).
    * Coercion runs user code at compile time, which Girard's paradox allows to diverge; the termination guard is deferred
    * with the recursion/effect model.
    */
  private def coercionPayload(
      implFqn: ValueFQN,
      actual: SemValue,
      expected: SemValue,
      context: Sourced[String]
  ): CheckIO[Option[SemValue]] =
    for {
      orvOpt <- liftF(getFactIfProduced(OperatorResolvedValue.Key(implFqn, platform)))
      result <- orvOpt.flatMap(orv => orv.runtime.map((orv, _))) match {
                  case None              => pure(Option.empty[SemValue])
                  case Some((orv, body)) =>
                    for {
                      sigSem        <- evalExpr(orv.typeStack.value.signature, Some(Env.empty))
                      (mono, binds) <- peelSigMetas(sigSem, Seq.empty)
                      _             <- doUnify(mono, VPi(actual, _ => expected), context)
                      _             <- modify(st => st.withUnifier(st.unifier.drain()))
                      concreteEnv   <- binds.foldLeftM(Env.empty) { case (env, (name, meta)) =>
                                         force(meta).map(env.bind(name, _))
                                       }
                      bodyVal       <- evalExpr(body.value, Some(concreteEnv))
                      applied        = Evaluator.applyValue(bodyVal, VNeutral(NeutralHead.Reserved(NeutralHead.Marker.Coerce), Spine.SNil))
                      forced        <- force(applied)
                      // The guard already decided applicability (a non-fitting range found no instance), so a resolved
                      // instance always yields a real conversion: the total `coerce` body evaluated with its value
                      // argument left as the marker — `nativeWiden(marker)` — which is the payload to splice.
                    } yield Some(forced)
                }
    } yield result


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
