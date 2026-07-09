package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** The ability-resolution *saturation* concern (the fourth checker collaborator, symmetrical with `solver` / `carriers`
  * / `calcReturns`): discovering every ability-qualified value reference and resolving each to its concrete impl,
  * factored out of the [[TypeStackLoop]]'s post-drain fold. None of this is definitional equality: it re-enters
  * `getFact` to read an ability marker's arity and to look up an impl's associated-type bindings, and it drives the
  * `resolve-abilities` fixed-point pass.
  *
  * Two entry points, mirroring the two phases:
  *   - [[collectAbilityRefs]] — walk the checker's output trees to find every ability-qualified reference (called from
  *     `TypeStackLoop.processIO` to seed the pass context).
  *   - [[resolveAbilities]] — the `resolve-abilities` saturation pass body: try to resolve every still-unresolved
  *     reference once, recording each impl and injecting its associated types into their standing metas.
  *
  * The resolutions themselves live in the shared [[CheckState.abilityResolutions]] map — this module only *records*
  * into it. Operates over [[CheckIO]], reading and writing the shared [[CheckState]] through `get`/`modify`/`inspect`.
  * It depends on exactly the primitives passed at construction — that narrow surface is the module boundary.
  *
  * @param resolveAbility
  *   Resolve an ability at concrete type arguments to its impl — the checker's `resolveAbility`.
  * @param fetchBinding
  *   Fetch a value's evaluator [[SemValue]] binding — the checker's `fetchBinding`, used to read an impl's
  *   associated-type values for injection.
  * @param platform
  *   The track's platform, used to key the ability-marker signature fact.
  */
class AbilityResolver(
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    platform: Platform
) {
  import AbilityResolver.AbilityRef

  /** Walk a [[SemExpression]] tree and collect all ability-qualified [[SemExpression.ValueReference]] nodes with their
    * current type arguments. Non-ability references and other shapes are skipped.
    */
  def collectAbilityRefs(expr: Sourced[SemExpression]): Seq[AbilityRef] = {
    def go(se: SemExpression): Seq[AbilityRef] = se.expression match {
      case SemExpression.ValueReference(vfqn, typeArgs)        =>
        vfqn.value.name.qualifier match {
          case _: Qualifier.Ability => Seq((vfqn, typeArgs))
          case _                    => Seq.empty
        }
      case SemExpression.FunctionApplication(target, argument) =>
        go(target.value) ++ go(argument.value)
      case SemExpression.FunctionLiteral(_, _, body)           =>
        go(body.value)
      case _                                                   => Seq.empty
    }
    go(expr.value)
  }

  /** Saturation pass: try to resolve every still-unresolved ability reference once. Returns `true` iff at least one
    * ability newly resolved this call — a resolution records the impl and injects its associated-type values into
    * their standing metas, which the next round's drain propagates.
    */
  def resolveAbilities(
      allRefs: Seq[AbilityRef],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CheckIO[Boolean] =
    for {
      state      <- get
      unresolved  = allRefs.filterNot { case (ref, _) => state.abilityResolutions.contains(ref) }
      progressed <- unresolved.foldLeftM(false) { (acc, ref) =>
                      tryResolveOne(ref, paramConstraints).map(_ || acc)
                    }
    } yield progressed

  /** Try to resolve a single ability reference. Returns `true` iff the reference got newly resolved this call.
    *
    *   - If the ref is covered by an in-scope parameter constraint (e.g. calling a method of `Foo[T]` inside a `[T]
    *     with Foo[T]` context), the constraint's own type arguments are used — at monomorphization time they're already
    *     instantiated to the caller's concrete types, whereas the reference's implicit metas won't be solved until
    *     unification connects them. The constraint path is the direct way to get concrete args.
    *   - Otherwise the ref's own type arguments are used.
    *   - Quoting failure (still-unsolved metas) leaves the ref pending for the next iteration.
    *   - `resolveAbility` returning `None` at *ground* arguments is a failed **demand**: the
    *     [[AbilityImplementation]] fact's non-resolved outcome is read back and reported here, at this reference —
    *     the demand site — and the check aborts (see [[reportFailedDemand]]). Only an *absent* fact (the resolution
    *     aborted on an upstream error, already reported at its own definition) leaves the ref silently pending, to be
    *     emitted by [[PostDrainQuoter]] as an abstract reference.
    */
  private def tryResolveOne(
      ref: AbilityRef,
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CheckIO[Boolean] = {
    val (abilityVfqn, refTypeArgs) = ref
    abilityVfqn.value.name.qualifier match {
      case Qualifier.Ability(abilityName) =>
        for {
          state      <- get
          // The ability query must carry ONLY the ability-level type arguments. A method reference's own type args are
          // `[abilityParams ++ methodParams]` (the ability's params are prepended to every method signature), so we
          // slice off the method-level params, keeping the leading `abilityArity` (e.g. `flatMap` of `Monad[F[_]]`
          // queries `Monad[F]`, not `Monad[F, A, B]`).
          arity      <- abilityArity(abilityVfqn.value, abilityName)
          refSliced   = refTypeArgs.take(arity)
          refGroundE  = refSliced.toList.traverse(a => Quoter.quote(0, a, state.unifier.metaStore))
          // Prefer the reference's OWN type arguments once they are fully ground: they pin the exact implementation.
          // This is essential when several constraints share one ability — e.g. `Dep[Database]` and `Dep[Logger]` in a
          // single body are both `Dep`, so the by-name constraint lookup would always return the first and every `get`
          // would mis-dispatch to it. The constraint path remains the fallback *while the ref's args are still unsolved
          // metas* (its original rationale: at the use site the caller has already instantiated the constraint to
          // concrete types, whereas the reference's implicit metas are solved only once unification connects them).
          groundArgsE = refGroundE match {
                          case Right(ground) => Right(ground)
                          case Left(_)       =>
                            state.findConstraintTypeArgs(paramConstraints, abilityName) match {
                              case Some(cArgs) =>
                                cArgs.toList.traverse(a => Quoter.quote(0, a, state.unifier.metaStore))
                              case None        => refGroundE
                            }
                        }
          progressed <- groundArgsE match {
                          case Right(groundArgs) =>
                            for {
                              resolved <- liftF(resolveAbility(abilityVfqn.value, groundArgs))
                              stepped  <- resolved match {
                                            case Some(impl) =>
                                              for {
                                                _ <- modify(_.recordAbilityResolution(abilityVfqn, impl))
                                                _ <- injectForImpl(
                                                       abilityName,
                                                       impl._1,
                                                       impl._2,
                                                       state.refAssocMetas.get(abilityVfqn),
                                                       abilityVfqn
                                                     )
                                              } yield true
                                            case None       => reportFailedDemand(abilityVfqn, abilityName, groundArgs)
                                          }
                            } yield stepped
                          case Left(_)           => pure(false)
                        }
        } yield progressed
      case _                              => pure(false)
    }
  }

  /** A ground-argument demand did not resolve. The producer ([[com.vanillasource.eliot.eliotc.ability.processor.AbilityImplementationProcessor]])
    * registers the failed [[AbilityImplementation.Resolution]] outcome on the fact instead of erroring — the position
    * belongs to the demander, and this saturation pass is the demander: it holds the demanding reference, so the
    * failure is reported *here*, at the use site, and the value's monomorphization aborts (the hard error at the
    * manifest instantiation, ability-guards §3). An **absent** fact means the resolution itself aborted on an upstream
    * error already reported at its own definition, so nothing is added and the reference stays silently pending. A
    * `Resolved` outcome cannot reach this method (the `resolveAbility` callback returns it), but is left pending for
    * the next round rather than treated as an error, keeping the match total and fail-safe.
    */
  private def reportFailedDemand(
      ref: Sourced[ValueFQN],
      abilityName: String,
      groundArgs: Seq[GroundValue]
  ): CheckIO[Boolean] =
    liftF(getFactIfProduced(AbilityImplementation.Key(ref.value, groundArgs, platform))).flatMap { factOpt =>
      val argsShown = groundArgs.map(_.show).mkString("[", ", ", "]")
      factOpt.map(_.resolution) match {
        case None                                                        => pure(false)
        case Some(AbilityImplementation.Resolution.Resolved(_, _))       => pure(false)
        case Some(AbilityImplementation.Resolution.NoImplementation)     =>
          liftF(
            compilerError(
              ref.as(s"No ability implementation found for ability '$abilityName' with type arguments $argsShown.")
            ) >> abort[Boolean]
          )
        case Some(AbilityImplementation.Resolution.Rejected(messages))   =>
          liftF(messages.traverse_(message => compilerError(ref.as(message))) >> abort[Boolean])
        case Some(AbilityImplementation.Resolution.Ambiguous)            =>
          liftF(
            compilerError(
              ref.as(
                s"Multiple ability implementations found for ability '$abilityName' with type arguments $argsShown."
              )
            ) >> abort[Boolean]
          )
      }
    }

  /** The number of ability-level type parameters of the ability owning `methodVfqn` — read off the ability *marker*'s
    * signature (the synthetic value named after the ability, sharing the method's `Ability(name)` qualifier), whose
    * leading generic binders are exactly the ability's parameters. Used to slice an ability method reference's full
    * type args down to the ability-level prefix for the impl query. Falls back to `Int.MaxValue` (slice nothing) if the
    * marker has no resolvable signature, preserving prior behaviour rather than mis-slicing.
    */
  private def abilityArity(methodVfqn: ValueFQN, abilityName: String): CheckIO[Int] = {
    val markerFqn = ValueFQN(methodVfqn.moduleName, QualifiedName(abilityName, methodVfqn.name.qualifier))
    liftF(getFactIfProduced(OperatorResolvedValue.Key(markerFqn, platform))).map {
      case Some(orv) =>
        OperatorResolvedExpression.SignatureView.of(orv.typeStack.as(orv.typeStack.value.signature)).binders.length
      case None      => Int.MaxValue
    }
  }

  /** Reduce every *applied* abstract associated-type form inside a semantic value through its resolved ability
    * implementation. `AddResult[Int[0, 1], Int[2, 3]]` — the ability member as a first-class type function, applied to
    * ground ability arguments — resolves the `Arithmetic[Int[0, 1], Int[2, 3]]` implementation and reduces to that
    * impl's associated-type binding applied to the matched impl generics (`Int[2, 4]`), recursively (a formula like
    * `Interval[AddResult[S1, S2], AddResult[E1, E2]]` reduces its nested applications too). Two head shapes carry such
    * an application: an applied [[MetaRole.AbstractAssoc]] metavariable (an in-session reference, created by
    * `Checker.ensureBinding`) and an applied body-less [[VTopDef]] with an abstract-ability-type FQN (the
    * missing-binding fallback inside a fetched impl binding).
    *
    * An application whose ability-arity argument prefix is not yet ground is left stuck (rebuilt with reduced
    * arguments) — it reduces on a later pass once metavariables solve, or fails loudly at read-back. Ground arguments
    * with no resolvable implementation are a failed demand, reported here at the demanding position
    * ([[reportFailedDemand]]). Every successful reduction is recorded in [[CheckState.assocReductionCache]] for the
    * pure post-drain substitution ([[substituteAssocReductions]]).
    *
    * `VPi` codomains (Scala closures) cannot be rewritten by this IO walk — only the domain is reduced here. A
    * codomain-buried application is reduced when it *surfaces* (the body check applies the codomain, re-entering this
    * reducer through `Checker.check`), and read back through the cache-driven substitution.
    */
  def reduceAssocApplications(v: SemValue, source: Sourced[?]): CheckIO[SemValue] =
    get.flatMap { state =>
      if (containsAssocApplication(v, state)) reduceAssoc(v, source) else pure(v)
    }

  /** Cheap pure pre-check for [[reduceAssocApplications]]: whether the value contains an applied abstract
    * associated-type form anywhere the IO reducer can reach (spines and `VPi` domains — deliberately mirroring the
    * reducer's reach, so a `false` here never skips a reducible form).
    */
  private def containsAssocApplication(v: SemValue, state: CheckState): Boolean = {
    def go(x: SemValue): Boolean = Evaluator.force(x, state.unifier.metaStore) match {
      case VMeta(id, spine)       =>
        val args = spine.toList
        (args.nonEmpty && state.unifier.roleOf(id.value).isInstanceOf[MetaRole.AbstractAssoc]) || args.exists(go)
      case VTopDef(fqn, _, spine) =>
        val args = spine.toList
        (args.nonEmpty && ValueFQN.isAbstractAbilityType(fqn)) || args.exists(go)
      case VStuckNative(_, spine) => spine.toList.exists(go)
      case VNeutral(_, spine)     => spine.toList.exists(go)
      case VPi(domain, _)         => go(domain)
      case _                      => false
    }
    go(v)
  }

  private def reduceAssoc(v: SemValue, source: Sourced[?]): CheckIO[SemValue] =
    get.flatMap { state =>
      Evaluator.force(v, state.unifier.metaStore) match {
        case VMeta(id, spine) if spine.toList.nonEmpty =>
          state.unifier.roleOf(id.value) match {
            case MetaRole.AbstractAssoc(fqn) => reduceAssocApplication(fqn, spine.toList, source)
            case _                           => rebuildSpine(VMeta(id, Spine.SNil), spine, source)
          }
        case VTopDef(fqn, None, spine) if ValueFQN.isAbstractAbilityType(fqn) && spine.toList.nonEmpty =>
          reduceAssocApplication(fqn, spine.toList, source)
        case VTopDef(fqn, cached, spine)               => rebuildSpine(VTopDef(fqn, cached, Spine.SNil), spine, source)
        case VStuckNative(fqn, spine)                  => rebuildSpine(VStuckNative(fqn, Spine.SNil), spine, source)
        case VNeutral(head, spine)                     => rebuildSpine(VNeutral(head, Spine.SNil), spine, source)
        case VPi(domain, codomain)                     => reduceAssoc(domain, source).map(VPi(_, codomain))
        case other                                     => pure(other)
      }
    }

  private def rebuildSpine(head: SemValue, spine: Spine, source: Sourced[?]): CheckIO[SemValue] =
    spine.toList.traverse(reduceAssoc(_, source)).map(_.foldLeft(head)(Evaluator.applyValue))

  /** Reduce one associated-type application `fqn[args...]`. The leading ability-arity prefix of the (recursively
    * reduced) arguments must quote to ground values for the implementation query; otherwise the application is left
    * stuck with its reduced arguments.
    */
  private def reduceAssocApplication(
      fqn: ValueFQN,
      args: List[SemValue],
      source: Sourced[?]
  ): CheckIO[SemValue] =
    for {
      reducedArgs <- args.traverse(reduceAssoc(_, source))
      arity       <- abilityNameOf(fqn).fold(pure(Int.MaxValue))(name => abilityArity(fqn, name))
      state       <- get
      stuck        = reducedArgs.foldLeft(VTopDef(fqn, None, Spine.SNil): SemValue)(Evaluator.applyValue)
      result      <- if (reducedArgs.length < arity) pure(stuck)
                     else
                       reducedArgs.take(arity).traverse(a => Quoter.quote(0, a, state.unifier.metaStore)) match {
                         case Left(_)           => pure(stuck)
                         case Right(groundArgs) =>
                           state.assocReductionCache.get((fqn, groundArgs)) match {
                             case Some(cached) =>
                               pure(reducedArgs.drop(arity).foldLeft(cached)(Evaluator.applyValue))
                             case None         =>
                               resolveAndProject(fqn, groundArgs, reducedArgs.drop(arity), stuck, source)
                           }
                       }
    } yield result

  /** Resolve the implementation for one ground associated-type application and project its binding. A resolvable impl
    * without a binding for this member mirrors [[injectForImpl]]'s silent skip (the member was never given a concrete
    * value — surfaced at read-back, never silently retyped). A ground application with no resolvable impl is a failed
    * demand, reported at this position.
    */
  private def resolveAndProject(
      fqn: ValueFQN,
      groundArgs: Seq[GroundValue],
      extraArgs: List[SemValue],
      stuck: SemValue,
      source: Sourced[?]
  ): CheckIO[SemValue] =
    liftF(resolveAbility(fqn, groundArgs)).flatMap {
      case Some((implFqn, implTypeArgs)) =>
        val implAssocFqn = ValueFQN(implFqn.moduleName, QualifiedName(fqn.name.name, implFqn.name.qualifier))
        liftF(fetchBinding(implAssocFqn)).flatMap {
          case None      => pure(stuck)
          case Some(sem) =>
            val applied = implTypeArgs.map(Evaluator.groundToSem).foldLeft(sem)(Evaluator.applyValue)
            for {
              reduced <- reduceAssoc(applied, source)
              _       <- if (extraArgs.isEmpty) modify(_.cacheAssocReduction(fqn, groundArgs, reduced)) else pure(())
            } yield extraArgs.foldLeft(reduced)(Evaluator.applyValue)
        }
      case None                          =>
        abilityNameOf(fqn) match {
          case Some(name) => reportFailedDemand(source.as(fqn), name, groundArgs).as(stuck)
          case None       => pure(stuck)
        }
    }

  private def abilityNameOf(fqn: ValueFQN): Option[String] = fqn.name.qualifier match {
    case Qualifier.Ability(name) => Some(name)
    case _                       => None
  }

  /** Pure post-drain substitution of already-reduced associated-type applications, from
    * [[CheckState.assocReductionCache]]. Unlike the IO reducer this walk *can* descend under binders (it rebuilds
    * `VPi`/`VLam` closures purely), so a codomain-buried application the body check already reduced — the impl
    * method's own published return — reads back concrete. An application not in the cache is left as-is: the strict
    * quoter rejects it loudly, never silently.
    */
  def substituteAssocReductions(v: SemValue): CheckIO[SemValue] =
    inspect(state => assocSubstitution(state)(v))

  /** The pure substitution function over a state snapshot — see [[substituteAssocReductions]]. Also handed to the
    * [[PostDrainQuoter]], where every quoted type slot (signature and body alike) passes through it.
    */
  def assocSubstitution(state: CheckState): SemValue => SemValue = {
    def lookup(fqn: ValueFQN, args: List[SemValue], emptyHead: SemValue): SemValue = {
      val rebuilt = args.foldLeft(emptyHead)(Evaluator.applyValue)
      args.traverse(a => Quoter.quote(0, a, state.unifier.metaStore).toOption) match {
        case Some(ground) => state.assocReductionCache.getOrElse((fqn, ground), rebuilt)
        case None         => rebuilt
      }
    }
    def go(x: SemValue): SemValue = Evaluator.force(x, state.unifier.metaStore) match {
      case VMeta(id, spine)          =>
        val args = spine.toList.map(go)
        state.unifier.roleOf(id.value) match {
          case MetaRole.AbstractAssoc(fqn) if args.nonEmpty => lookup(fqn, args, VMeta(id, Spine.SNil))
          case _                                            => args.foldLeft(VMeta(id, Spine.SNil): SemValue)(Evaluator.applyValue)
        }
      case VTopDef(fqn, None, spine) =>
        val args = spine.toList.map(go)
        if (ValueFQN.isAbstractAbilityType(fqn) && args.nonEmpty) lookup(fqn, args, VTopDef(fqn, None, Spine.SNil))
        else args.foldLeft(VTopDef(fqn, None, Spine.SNil): SemValue)(Evaluator.applyValue)
      case VStuckNative(fqn, spine)  =>
        spine.toList.map(go).foldLeft(VStuckNative(fqn, Spine.SNil): SemValue)(Evaluator.applyValue)
      case VNeutral(head, spine)     =>
        spine.toList.map(go).foldLeft(VNeutral(head, Spine.SNil): SemValue)(Evaluator.applyValue)
      case VPi(domain, codomain)     => VPi(go(domain), arg => go(codomain(arg)))
      case VLam(name, closure)       => VLam(name, arg => go(closure(arg)))
      case other                     => other
    }
    go
  }

  /** For every abstract associated type of the given ability, unify its standing meta against the concrete impl's
    * corresponding associated-type value (looked up via [[fetchBinding]]). Missing impl bindings are silently skipped —
    * they were never in use, so no constraint is needed.
    *
    * A parameterised implementation's associated type is a *function of the impl's leading generic parameters* — e.g.
    * `implement[L1,H1,L2,H2] Arithmetic[Int[L1,H1], Int[L2,H2]] { type AddResult = Int[add(L1,L2), add(H1,H2)] }`
    * desugars its `AddResult` to `AddResult[L1,H1,L2,H2] = Int[add(L1,L2), add(H1,H2)]`. It must therefore be **applied**
    * to the impl's matched ground arguments (`implTypeArgs`, one per impl generic, in declaration order) before it can
    * solve the standing 0-arity abstract-assoc meta; otherwise those generics leak into the quoted type (`Int(0,150, +4
    * stray metas)`). A non-parameterised impl (`implement AssociatedType[Name] { type MagicType = String }`) has no
    * ground args, so the fold is a no-op and the binding is unified as-is.
    */
  private def injectForImpl(
      abilityName: String,
      implFqn: ValueFQN,
      implTypeArgs: Seq[GroundValue],
      refAssocMetas: Option[Set[Int]],
      source: Sourced[?]
  ): CheckIO[Unit] =
    for {
      state  <- get
      // Solve only the abstract-assoc metas belonging to *this* reference (`refAssocMetas`, captured per reference in
      // `Checker.infer`), so nested calls to the same ability method resolve independently. A reference without a
      // recorded set (`None`) falls back to every abstract-assoc meta of the ability — the pre-per-reference behaviour,
      // correct whenever the associated type is instance-constant (e.g. a non-parameterised `type MagicType = String`).
      targets = state.unifier.metaRoles.toSeq.collect {
                  case (rawId, MetaRole.AbstractAssoc(absFqn))
                      if absFqn.name.qualifier == Qualifier.Ability(abilityName) &&
                        refAssocMetas.forall(_.contains(rawId)) =>
                    (absFqn, SemValue.MetaId(rawId))
                }
      _      <- targets.traverse_ { case (absFqn, metaId) =>
                  val implAssocFqn = ValueFQN(implFqn.moduleName, QualifiedName(absFqn.name.name, implFqn.name.qualifier))
                  liftF(fetchBinding(implAssocFqn)).flatMap {
                    case None      => pure(())
                    case Some(sem) =>
                      val applied = implTypeArgs
                        .map(Evaluator.groundToSem)
                        .foldLeft(sem)(Evaluator.applyValue)
                      for {
                        // A formula binding may itself contain associated-type *applications* (Interval's
                        // `Interval[AddResult[S1, S2], AddResult[E1, E2]]`) — now applied to ground impl generics, so
                        // reduce them through their own instances before solving the standing meta.
                        reduced <- reduceAssocApplications(applied, source)
                        _       <- modify { s =>
                                     s.withUnifier(
                                       s.unifier
                                         .unify(
                                           VMeta(metaId, Spine.SNil),
                                           Evaluator.force(reduced, s.unifier.metaStore),
                                           source.as(s"Associated type '${absFqn.name.name}' mismatch.")
                                         )
                                     )
                                   }
                      } yield ()
                  }
                }
    } yield ()
}

object AbilityResolver {

  /** An ability-qualified value reference paired with its current type arguments (unquoted [[SemValue]]s). */
  type AbilityRef = (Sourced[ValueFQN], Seq[SemValue])
}
