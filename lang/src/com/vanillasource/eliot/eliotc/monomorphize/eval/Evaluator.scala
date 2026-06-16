package com.vanillasource.eliot.eliotc.monomorphize.eval

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression

/** Pure, synchronous NbE evaluator. Evaluates ORE syntax into the semantic domain (SemValue).
  *
  * The evaluator always produces VLam for FunctionLiteral — the Checker is the only place that produces VPi.
  *
  * @param lookupTopDef
  *   Function to look up a top-level definition by ValueFQN, returning its semantic value
  */
class Evaluator(
    lookupTopDef: ValueFQN => Option[SemValue]
) {

  /** Evaluate an ORE expression to a semantic value under the given environment. */
  def eval(env: Env, tm: OperatorResolvedExpression): SemValue = tm match {
    case OperatorResolvedExpression.IntegerLiteral(value) =>
      VConst(GroundValue.Direct(value.value, Evaluator.bigIntGroundType))

    case OperatorResolvedExpression.StringLiteral(value) =>
      VConst(GroundValue.Direct(value.value, Evaluator.stringGroundType))

    case OperatorResolvedExpression.ParameterReference(name) =>
      env
        .lookupByName(name.value)
        .getOrElse(
          VNeutral(NeutralHead.VVar(env.level, name.value), Spine.SNil)
        )

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      val base = lookupTopDef(vfqn.value) match {
        case Some(sem) => sem
        case None      => VNeutral(NeutralHead.VVar(env.level, vfqn.value.name.name), Spine.SNil)
      }
      // Thread explicit type arguments into the value's spine. For a type-application scrutinee like `Tag["hello"]`
      // this keeps `"hello"` in the constructor's spine, so a type-match `case Tag[name] -> name` can bind it.
      typeArgs.foldLeft(base)((acc, ta) => Evaluator.applyValue(acc, eval(env, ta.value)))

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      Evaluator.applyValue(eval(env, target.value), eval(env, arg.value))

    case OperatorResolvedExpression.FunctionLiteral(paramName, _, body) =>
      VLam(paramName.value, arg => eval(env.bind(paramName.value, arg), body.value))
  }
}

object Evaluator {

  /** Apply a semantic value to an argument. */
  def applyValue(f: SemValue, x: SemValue): SemValue = f match {
    case VLam(_, closure) => closure(x)

    case VPi(_, codomain) => codomain(x)

    case VNative(_, fire) =>
      // Always fire — even on a non-concrete argument. The native's `fire` produces its own canonical stuck form
      // (a body-less `VTopDef` carrying the native's FQN and the renormalised spine) when an argument is not yet
      // concrete, so distinct stuck natives stay definitionally distinct and `renormalize` can re-fire them once the
      // arguments solve. (A previous neutral-only special case collapsed every native to a single anonymous
      // `VVar(0, "native")` head, which conflated, e.g., `add(x, y)` with `subtract(x, y)` under definitional
      // equality.) Firing on a neutral mirrors firing on a concrete value: a curried native simply yields the next
      // `VNative` awaiting the remaining arguments.
      fire(unfoldTopDef(x))

    case VNeutral(head, spine) => VNeutral(head, spine :+ x)

    case VTopDef(fqn, cached, spine) => VTopDef(fqn, cached, spine :+ x)

    case VMeta(id, spine) => VMeta(id, spine :+ x)

    case _ => x // fallback — should not happen in well-typed programs
  }

  /** Unfold a VTopDef by evaluating its cached body and applying the spine. Does not require MetaStore — used at
    * evaluation time before metas exist.
    */
  private def unfoldTopDef(v: SemValue): SemValue = v match {
    case VTopDef(_, Some(cached), spine) =>
      val base   = cached.value
      val result = spine.toList.foldLeft(base)(applyValue)
      unfoldTopDef(result)
    case _                               => v
  }

  /** Deeply normalise a semantic value, **re-firing stuck native applications** whose arguments have since become
    * concrete.
    *
    * A native (e.g. `add`/`min`/`lessThanOrEqual`) applied to an argument that was a still-unsolved metavariable at
    * evaluation time goes stuck as a body-less `VTopDef(fqn, None, spine)` (its `fire` returns this canonical stuck
    * form) — and ordinary [[force]] never re-fires it, because the stuck form has dropped the `VNative` reducer and
    * `force` only unfolds *cached* bodies. This bites dependent-bounds arithmetic: `Int[LMin,LMax] + Int[RMin,RMax]`
    * has result type `Int[add(LMin,RMin), add(LMax,RMax)]`, and when that codomain is computed during application
    * inference the bound metavariables are not yet solved, so each `add(?,?)` sticks. By the time the inferred type is
    * compared against the expected type the metavariables *are* solved, but the `add`s never reduced.
    *
    * `renormalize` walks the value, [[force]]ing through solved metas, and for each body-less `VTopDef` whose FQN
    * resolves (via `lookupNative`) to a [[VNative]] it re-applies the native to the renormalised spine — so a
    * now-fully- concrete `add(3, 4)` reduces to `7`. If re-firing still produces the same stuck head (an argument is
    * genuinely still abstract), the stuck form is kept (with renormalised arguments) and no progress loops. Non-native
    * body-less `VTopDef`s (type constructors like `Int`, abstract `def`s) are rebuilt with renormalised arguments;
    * everything else is returned as forced.
    *
    * `lookupNative` is the checker's binding cache (`vfqn => bindingCache.getOrElse(vfqn, None)`), which already holds
    * every native reachable from the term (prefetched before evaluation).
    */
  def renormalize(v: SemValue, metaStore: MetaStore, lookupNative: ValueFQN => Option[SemValue]): SemValue =
    force(v, metaStore) match {
      case VTopDef(fqn, None, spine) =>
        val args    = spine.toList.map(renormalize(_, metaStore, lookupNative))
        val rebuilt = args.foldLeft(VTopDef(fqn, None, Spine.SNil): SemValue)(applyValue)
        lookupNative(fqn) match {
          case Some(native: VNative) =>
            args.foldLeft(native: SemValue)(applyValue) match {
              // Still stuck on the same native (an argument is genuinely abstract) — keep the rebuilt stuck form.
              case VTopDef(stuckFqn, None, _) if stuckFqn == fqn => rebuilt
              case fired                                         => renormalize(fired, metaStore, lookupNative)
            }
          case _                     => rebuilt
        }
      case forced                    => forced
    }

  /** Force a semantic value by walking solved metas and unfolding VTopDef. */
  def force(v: SemValue, metaStore: MetaStore): SemValue = v match {
    case VMeta(id, spine)                =>
      metaStore.lookup(id) match {
        case Some(solved) =>
          val base = force(solved, metaStore)
          spine.toList.foldLeft(base)(applyValue)
        case None         => v
      }
    case VTopDef(_, Some(cached), spine) =>
      val base   = force(cached.value, metaStore)
      val result = spine.toList.foldLeft(base)(applyValue)
      force(result, metaStore)
    case _                               => v
  }

  import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes

  /** Convert a SemValue to a GroundValue. Used by native fire functions that need to store type arguments in
    * GroundValue structures.
    */
  def semToGround(v: SemValue): GroundValue = v match {
    case VConst(g)                       => g
    case VType                           => GroundValue.Type
    case VPi(domain, codomain)           =>
      val domGround = semToGround(domain)
      val codGround = semToGround(codomain(VNeutral(NeutralHead.VVar(0, "$quote"), Spine.SNil)))
      GroundValue.Structure(WellKnownTypes.functionDataTypeFQN, Seq(domGround, codGround), GroundValue.Type)
    case VTopDef(fqn, None, spine)       =>
      GroundValue.Structure(fqn, spine.toList.map(semToGround), GroundValue.Type)
    case VTopDef(_, Some(cached), spine) =>
      val base   = cached.value
      val result = spine.toList.foldLeft(base)(applyValue)
      semToGround(result)
    case _                               => GroundValue.Type
  }

  /** Convert a GroundValue to a SemValue. Inverse of [[semToGround]] for [[GroundValue.Structure]]s: the head FQN
    * becomes a [[VTopDef]] and the positional args become a spine in order. `GroundValue.Type` maps to [[VType]]. All
    * other ground values wrap as [[VConst]].
    *
    * Used by ability pattern matching so that concrete query-side ground arguments participate in structural
    * unification against pattern-side [[SemValue]]s produced by evaluating marker-function ORE signatures.
    */
  def groundToSem(g: GroundValue): SemValue = g match {
    case GroundValue.Type                    => VType
    case GroundValue.Structure(fqn, args, _) =>
      args.map(groundToSem).foldLeft(VTopDef(fqn, None, Spine.SNil): SemValue)(applyValue)
    case _                                   => VConst(g)
  }

  /** Ground type for BigInteger values (used by eval for IntegerLiteral). */
  val bigIntGroundType: GroundValue =
    GroundValue.Structure(WellKnownTypes.bigIntFQN, Seq.empty, GroundValue.Type)

  /** Ground type for String values (used by eval for StringLiteral). */
  val stringGroundType: GroundValue =
    GroundValue.Structure(WellKnownTypes.stringFQN, Seq.empty, GroundValue.Type)

  /** Ground type for Bool values (used by the compile-time Bool natives). */
  val boolGroundType: GroundValue =
    GroundValue.Structure(WellKnownTypes.boolFQN, Seq.empty, GroundValue.Type)

  /** Compile-time Bool values. The runtime representation is platform-dependent (the JVM backend maps Bool to a
    * platform boolean); these are the values the NbE evaluator reduces type-level predicates to.
    */
  val trueValue: SemValue  = VConst(GroundValue.Direct(true, boolGroundType))
  val falseValue: SemValue = VConst(GroundValue.Direct(false, boolGroundType))
}
