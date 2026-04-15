package com.vanillasource.eliot.eliotc.ability.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Structural ability-pattern matching via the monomorphize NbE unifier.
  *
  * A query evaluates one or more marker-function ORE signatures into the semantic domain, treating every abstract
  * ability-typed reference as a fresh [[VMeta]] and every type constructor as a [[VTopDef]] head. Leading
  * type-parameter lambdas are peeled into fresh metas and the remaining curried function type is walked to
  * extract pattern arguments. Matching and overlap questions then reduce to [[Unifier]] calls.
  *
  * All ability pattern questions — impl matching at call sites, overlap detection at definition time, and
  * signature compatibility — route through this module.
  */
object AbilityMatcher {

  /** Match an impl's pattern (given as its marker-function ORE signature) against a concrete tuple of query type
    * arguments. On a match, returns the GroundValue each of the impl's leading type parameters was bound to, in
    * declaration order; returns None when there is no match.
    */
  def matchImpl(
      markerSignature: Sourced[OperatorResolvedExpression],
      queryArgs: Seq[GroundValue]
  ): CompilerIO[Option[Seq[GroundValue]]] =
    for {
      setup <- setupEvaluation(Seq(markerSignature))
      peeled = peelPattern(setup, markerSignature)
    } yield attemptMatch(peeled, queryArgs, markerSignature)

  /** Do two impl marker patterns overlap — i.e. would some concrete query tuple match both? Used for
    * definition-time overlap detection on implementations of the same ability in the same module.
    */
  def patternsOverlap(
      sigA: Sourced[OperatorResolvedExpression],
      sigB: Sourced[OperatorResolvedExpression]
  ): CompilerIO[Boolean] =
    for {
      setup <- setupEvaluation(Seq(sigA, sigB))
      peeledA = peelPattern(setup, sigA)
      peeledB = peelPattern(peeledA.setup, sigB)
    } yield attemptOverlap(peeledB.setup, peeledA.args, peeledB.args, sigA)

  /** Check that an impl method's signature is compatible with the ability's abstract method signature.
    *
    * The abstract method's leading type parameters (both ability-level and method-level) are zipped with the impl
    * marker's pattern arguments: the ability-level params get bound to the pattern args, and any remaining
    * method-level params stay as free metas that unify with the impl side's method-level metas by position.
    *
    * The impl method body is then unified against the (substituted) abstract method body.
    */
  def signaturesCompatible(
      abstractMethodSig: Sourced[OperatorResolvedExpression],
      implMethodSig: Sourced[OperatorResolvedExpression],
      implMarkerSig: Sourced[OperatorResolvedExpression]
  ): CompilerIO[Boolean] =
    for {
      setup                         <- setupEvaluation(Seq(abstractMethodSig, implMethodSig, implMarkerSig))
      (s1, absParams, absBody)       = peelToBody(setup, abstractMethodSig)
      (s2, _, implBody)              = peelToBody(s1, implMethodSig)
      marker                         = peelPattern(s2, implMarkerSig)
    } yield attemptSigCompat(marker.setup, absParams, marker.args, absBody, implBody, abstractMethodSig)

  // ---- Phase 1: classify every transitively-referenced ORE value ----

  private sealed trait Binding
  private object Binding {
    case object Constructor                                    extends Binding
    case object AbstractAbility                                extends Binding
    case class Body(body: Sourced[OperatorResolvedExpression]) extends Binding
  }

  private def collectBindings(
      expr: OperatorResolvedExpression,
      acc: Map[ValueFQN, Binding]
  ): CompilerIO[Map[ValueFQN, Binding]] =
    OperatorResolvedExpression.foldValueReferences(expr, acc) { (map, name) =>
      if (map.contains(name.value) || name.value == WellKnownTypes.typeFQN) map.pure[CompilerIO]
      else classifyValueRef(name.value, name, map)
    }

  private def classifyValueRef(
      vfqn: ValueFQN,
      sourceRef: Sourced[ValueFQN],
      acc: Map[ValueFQN, Binding]
  ): CompilerIO[Map[ValueFQN, Binding]] =
    getFact(OperatorResolvedValue.Key(vfqn)).flatMap {
      case None       =>
        vfqn.name.qualifier match {
          case Qualifier.Type => (acc + (vfqn -> Binding.Constructor)).pure[CompilerIO]
          case _              => compilerAbort[Map[ValueFQN, Binding]](sourceRef.as("Can not evaluate referenced value."))
        }
      case Some(fact) =>
        fact.runtime match {
          case Some(body) =>
            // Recurse into the fetched body to discover further references; the already-in-map check in
            // `collectBindings` guards against cycles because we add the Body entry before recursing.
            collectBindings(body.value, acc + (vfqn -> Binding.Body(body)))
          case None       =>
            vfqn.name.qualifier match {
              case _: Qualifier.Ability | _: Qualifier.AbilityImplementation =>
                (acc + (vfqn -> Binding.AbstractAbility)).pure[CompilerIO]
              case _                                                         =>
                (acc + (vfqn -> Binding.Constructor)).pure[CompilerIO]
            }
        }
    }

  // ---- Phase 2: build a pure Evaluator over the classifications ----

  private case class Setup(evaluator: Evaluator, metaStore: MetaStore)

  private def setupEvaluation(sigs: Seq[Sourced[OperatorResolvedExpression]]): CompilerIO[Setup] =
    sigs.foldLeft(Map.empty[ValueFQN, Binding].pure[CompilerIO])((acc, sig) =>
      acc.flatMap(collectBindings(sig.value, _))
    ).map(buildSetup)

  private def buildSetup(classifications: Map[ValueFQN, Binding]): Setup = {
    // Pre-allocate a stable meta per abstract ability-typed reference so that multiple references to the same
    // abstract associated type unify with each other without a substitution pass.
    val (store0, abilityMetaIds) = classifications.foldLeft((MetaStore.empty, Map.empty[ValueFQN, MetaId])) {
      case ((s, ids), (vfqn, Binding.AbstractAbility)) =>
        val (id, next) = s.fresh
        (next, ids + (vfqn -> id))
      case (acc, _)                                    => acc
    }

    // Mutual recursion: bindings for runtime-bodied values evaluate their body through `evaluator`, and a `Lazy`
    // defers evaluation until the VTopDef is forced. Both vals resolve in any order at first access.
    lazy val bindings: Map[ValueFQN, SemValue]      = classifications.map {
      case (vfqn, Binding.Constructor)     => vfqn -> VTopDef(vfqn, None, Spine.SNil)
      case (vfqn, Binding.AbstractAbility) => vfqn -> VMeta(abilityMetaIds(vfqn), Spine.SNil)
      case (vfqn, Binding.Body(body))      =>
        vfqn -> VTopDef(vfqn, Some(Lazy(evaluator.eval(Env.empty, body.value))), Spine.SNil)
    } + (WellKnownTypes.typeFQN -> VType)
    lazy val evaluator: Evaluator                    = new Evaluator(vfqn => bindings.get(vfqn))

    Setup(evaluator, store0)
  }

  // ---- Phase 3: peel leading VLams into metas and extract pattern args ----

  private case class Peeled(
      setup: Setup,
      paramMetas: Seq[(String, MetaId)],
      args: Seq[SemValue]
  )

  private def peelPattern(setup: Setup, sig: Sourced[OperatorResolvedExpression]): Peeled = {
    val evaluated                     = setup.evaluator.eval(Env.empty, sig.value)
    val (body, nextStore, paramMetas) = peelLambdas(evaluated, setup.metaStore, Seq.empty)
    val args                          = extractFunctionArgs(body, nextStore)
    Peeled(Setup(setup.evaluator, nextStore), paramMetas, args)
  }

  /** Peel leading lambdas but return the whole body (not decomposed into function args). Used for signature
    * compatibility checks where the body is unified whole.
    */
  private def peelToBody(
      setup: Setup,
      sig: Sourced[OperatorResolvedExpression]
  ): (Setup, Seq[(String, MetaId)], SemValue) = {
    val evaluated                     = setup.evaluator.eval(Env.empty, sig.value)
    val (body, nextStore, paramMetas) = peelLambdas(evaluated, setup.metaStore, Seq.empty)
    (Setup(setup.evaluator, nextStore), paramMetas, body)
  }

  @scala.annotation.tailrec
  private def peelLambdas(
      sv: SemValue,
      metaStore: MetaStore,
      acc: Seq[(String, MetaId)]
  ): (SemValue, MetaStore, Seq[(String, MetaId)]) =
    Evaluator.force(sv, metaStore) match {
      case VLam(name, closure) =>
        val (id, nextStore) = metaStore.fresh
        val metaVal         = VMeta(id, Spine.SNil)
        peelLambdas(closure(metaVal), nextStore, acc :+ (name, id))
      case other               => (other, metaStore, acc)
    }

  /** Walk a curried function type (a [[VTopDef]] at the Function FQN with spine `[paramType, returnType]`) and
    * collect argument positions, dropping the final return type.
    */
  private def extractFunctionArgs(sv: SemValue, metaStore: MetaStore): Seq[SemValue] =
    Evaluator.force(sv, metaStore) match {
      case VTopDef(fqn, _, spine) if fqn == WellKnownTypes.functionDataTypeFQN =>
        spine.toList match {
          case paramType :: returnType :: Nil => paramType +: extractFunctionArgs(returnType, metaStore)
          case _                              => Seq.empty
        }
      case _                                                                   => Seq.empty
    }

  // ---- Matching and overlap, delegating to Unifier ----

  private def attemptMatch(
      peeled: Peeled,
      queryArgs: Seq[GroundValue],
      sourceRef: Sourced[?]
  ): Option[Seq[GroundValue]] =
    if (peeled.args.size != queryArgs.size) None
    else {
      val querySems = queryArgs.map(Evaluator.groundToSem)
      val context   = sourceRef.as("ability-match")
      val unified   = peeled.args.zip(querySems).foldLeft(Unifier.create(peeled.setup.metaStore, 0)) {
        case (u, (pat, q)) => u.unify(pat, q, context)
      }.drain()
      if (unified.errors.nonEmpty) None
      else {
        // For each impl type parameter meta, prefer its original query slot (walked structurally from the pattern
        // tree alongside the raw GroundValue) so that data-type field naming is preserved verbatim. Fall back to
        // quoting the metastore for cases we can't structurally trace (runtime-bodied aliases, etc.).
        val traced: Map[MetaId, GroundValue] =
          peeled.args.zip(queryArgs).flatMap { case (p, q) => tracePatternMetas(p, q) }.toMap
        Some(peeled.paramMetas.map { case (_, id) =>
          traced.getOrElse(id, metaToGround(id, unified.metaStore))
        })
      }
    }

  /** Walk a pattern [[SemValue]] alongside the original query [[GroundValue]] that it was unified against, and
    * record every [[MetaId]] → [[GroundValue]] pairing that appears at a structurally-aligned position.
    *
    * Preserves the exact query-side ground values on complex bindings rather than re-quoting the meta's solution
    * through the metastore, so the returned GroundValue matches the one present on the call site.
    */
  private def tracePatternMetas(
      pattern: SemValue,
      query: GroundValue
  ): Seq[(MetaId, GroundValue)] = pattern match {
    case VMeta(id, Spine.SNil)  =>
      Seq(id -> query)
    case VTopDef(fqn, _, spine) =>
      query match {
        case GroundValue.Structure(queryFqn, args, _) if queryFqn == fqn =>
          spine.toList.zip(args).flatMap { case (s, g) => tracePatternMetas(s, g) }
        case _                                                           => Seq.empty
      }
    case _                      => Seq.empty
  }

  private def attemptSigCompat(
      setup: Setup,
      absParams: Seq[(String, MetaId)],
      markerArgs: Seq[SemValue],
      absBody: SemValue,
      implBody: SemValue,
      sourceRef: Sourced[?]
  ): Boolean = {
    val context = sourceRef.as("ability-sig-compat")
    // Bind the leading (ability-level) abstract type params to the impl marker's pattern args via unification.
    // Any remaining abstract params (method-level) stay as free metas and will unify with the impl method's
    // corresponding metas during body unification.
    val bound   = absParams.zip(markerArgs).foldLeft(Unifier.create(setup.metaStore, 0)) {
      case (u, ((_, absMetaId), patternArg)) =>
        u.unify(VMeta(absMetaId, Spine.SNil), patternArg, context)
    }
    bound.unify(absBody, implBody, context).drain().errors.isEmpty
  }

  private def attemptOverlap(
      setup: Setup,
      argsA: Seq[SemValue],
      argsB: Seq[SemValue],
      sourceRef: Sourced[?]
  ): Boolean =
    if (argsA.size != argsB.size) false
    else {
      val context = sourceRef.as("ability-overlap")
      val unified = argsA.zip(argsB).foldLeft(Unifier.create(setup.metaStore, 0)) {
        case (u, (a, b)) => u.unify(a, b, context)
      }.drain()
      unified.errors.isEmpty
    }

  /** Read back a meta's solution as a GroundValue, mirroring the previous implementation's behaviour of defaulting
    * complex (non-simple-constructor, non-primitive) bindings to [[GroundValue.Type]].
    */
  private def metaToGround(id: MetaId, metaStore: MetaStore): GroundValue =
    metaStore.lookup(id) match {
      case None      => GroundValue.Type
      case Some(sem) =>
        Evaluator.force(sem, metaStore) match {
          case VType                       => GroundValue.Type
          case VConst(g)                   => g
          case VTopDef(fqn, _, Spine.SNil) =>
            GroundValue.Structure(fqn, Seq.empty, GroundValue.Type)
          case _                           => GroundValue.Type
        }
    }
}
