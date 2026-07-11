package com.vanillasource.eliot.eliotc.reconcile.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.reconcile.fact.{ReconciledMonomorphicExpression, ReconciledMonomorphicValue}
import com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicExpression.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.uncurry.fact.{UncurriedMonomorphicExpression, UncurriedMonomorphicValue}

/** The representation-reconcile pass (`docs/bounds-as-refinements.md`): a platform-generic post-pass over
  * [[UncurriedMonomorphicValue]] that enriches the body so the backend can lay out each `Int` from the refinement
  * channel with no width policy of its own.
  *
  * It does two things, both pure meta bookkeeping (no width / representation knowledge — that stays in the backend):
  *
  *   1. **Stamps each node's meta.** It reads the instance's [[RefinementTable]] and attaches the channel's per-node
  *      interval (as an `Interval` [[GroundValue]]) onto every node whose value range the channel pinned. A ⊤/unknown
  *      node gets [[None]] (the backend lays it out as a bignum).
  *   2. **Inserts [[ReconciledMonomorphicExpression.Reconcile]] nodes at meta-change edges.** Wherever a value flows into
  *      a context that fixes a *different* expected meta than the value's own, the value is wrapped so the backend emits
  *      a representation re-encode. The edges (see the class note in `docs/bounds-as-refinements.md` §4):
  *        - a **branch-arm merge** — each arm of a `Bool::fold` is reconciled to the fold node's own (joined) meta;
  *        - an **argument boundary** — each argument of an *ordinary* call/constructor is reconciled to the callee's
  *          declared parameter meta (⊤/bignum today, since `Int` parameters carry no declared range yet);
  *        - the **return boundary** — the whole body is reconciled to the declared return meta (⊤/bignum today).
  *
  * The **width-transparent intrinsics** are the exception: the arithmetic leaves (`nativeAdd`/`nativeSubtract`/
  * `nativeMultiply`), the ordering leaf (`intLessThanOrEqual`) and `intToString` consume their operands at whatever
  * width their meta gives (the backend unboxes each per-operand), so their operands are *not* reconciled — that would
  * pre-widen away the narrow representation. Their result meta is the channel's own entry for the call node (the
  * arithmetic transfer). `Bool::fold` is handled specially (arm merge above).
  *
  * A reconcile is inserted only when the value's own meta differs from the imposed expected meta (`Option[GroundValue]`
  * structural inequality). Because the channel only ever records intervals for `Int` nodes, a non-`Int` value has meta
  * [[None]] and is imposed [[None]], so it is never wrapped — the pass needs no type test.
  */
class ReconcileProcessor
    extends TransformationProcessor[UncurriedMonomorphicValue.Key, ReconciledMonomorphicValue.Key](key =>
      UncurriedMonomorphicValue.Key(key.vfqn, key.typeArguments, key.arity)
    )
    with Logging {

  import ReconcileProcessor.*

  override protected def generateFromKeyAndFact(
      key: ReconciledMonomorphicValue.Key,
      umv: UncurriedMonomorphicValue
  ): CompilerIO[ReconciledMonomorphicValue] =
    for {
      refinementTable <- getFactIfProduced(RefinementTable.Key(key.vfqn, key.typeArguments))
      metas            = refinementTable.map(metaByPosition).getOrElse(Map.empty[PositionRange, GroundValue])
      reconciledBody   = umv.body.map { b =>
                           val topNode = b.as(UncurriedMonomorphicExpression(umv.returnType, b.value))
                           // The whole body reconciles to the declared return meta (⊤/None today — `Int` returns carry
                           // no declared range yet), so a body computing a narrow value re-encodes to the bignum
                           // descriptor at the return.
                           reconcileTo(walk(topNode, metas), None)
                         }
    } yield ReconciledMonomorphicValue(
      vfqn = key.vfqn,
      typeArguments = key.typeArguments,
      arity = key.arity,
      name = umv.name,
      signature = umv.signature,
      parameters = umv.parameters,
      returnType = umv.returnType,
      body = reconciledBody
    )

  /** Walk one uncurried node into its reconciled form: stamp its channel meta and recurse, inserting reconciles at the
    * edges below. Pure — no fact reads.
    */
  private def walk(
      node: Sourced[UncurriedMonomorphicExpression],
      metas: Map[PositionRange, GroundValue]
  ): Sourced[ReconciledMonomorphicExpression] = {
    val own          = metas.get(node.range)
    val expressionType = node.value.expressionType
    val reconciled   = node.value.expression match {
      case UncurriedMonomorphicExpression.IntegerLiteral(value)          =>
        IntegerLiteral(value)
      case UncurriedMonomorphicExpression.StringLiteral(value)           =>
        StringLiteral(value)
      case UncurriedMonomorphicExpression.ParameterReference(name)       =>
        ParameterReference(name)
      case UncurriedMonomorphicExpression.MonomorphicValueReference(v, t) =>
        MonomorphicValueReference(v, t)
      case UncurriedMonomorphicExpression.FunctionLiteral(params, body)  =>
        // A lambda body is a ⊤ boundary (the channel records no interval inside it), so nothing narrow flows out; walk
        // it for structure only, no reconcile needed.
        FunctionLiteral(params, walk(body, metas))
      case UncurriedMonomorphicExpression.FunctionApplication(t, args)   =>
        val walkedTarget = walk(t, metas)
        calleeOf(t) match {
          case Some(fqn) if widthTransparentLeaves.contains(fqn)      =>
            // Arithmetic / ordering / toString: operands consumed at their own width by the leaf — do not reconcile.
            FunctionApplication(walkedTarget, args.map(walk(_, metas)))
          case Some(fqn) if fqn === boolFoldFqn && args.sizeIs == 3   =>
            // Branch merge: reconcile each arm to the fold node's own (joined) meta. The condition is a `Bool`, ⊤.
            val armExpected = own
            FunctionApplication(
              walkedTarget,
              Seq(
                walk(args(0), metas),
                reconcileTo(walk(args(1), metas), armExpected),
                reconcileTo(walk(args(2), metas), armExpected)
              )
            )
          case _                                                     =>
            // Ordinary call / constructor: every argument crosses a boundary to the callee's declared parameter meta
            // (⊤/None today), so reconcile each to None.
            FunctionApplication(walkedTarget, args.map(a => reconcileTo(walk(a, metas), None)))
        }
    }
    node.as(ReconciledMonomorphicExpression(own, expressionType, reconciled))
  }

  /** Wrap `node` in a [[ReconciledMonomorphicExpression.Reconcile]] targeting `expected` iff the node's own meta differs
    * from it; otherwise return it unchanged. The wrapper carries `expected` as its meta (the target), and its source is
    * the node itself (whose meta is the re-encode source).
    */
  private def reconcileTo(
      node: Sourced[ReconciledMonomorphicExpression],
      expected: Option[GroundValue]
  ): Sourced[ReconciledMonomorphicExpression] =
    if (node.value.meta === expected) node
    else node.as(ReconciledMonomorphicExpression(expected, node.value.expressionType, Reconcile(node)))

  /** The callee FQN of an application target, when it is a direct monomorphic value reference (the flattened head an
    * uncurried application always has for a named call/leaf).
    */
  private def calleeOf(target: Sourced[UncurriedMonomorphicExpression]): Option[ValueFQN] =
    target.value.expression match {
      case UncurriedMonomorphicExpression.MonomorphicValueReference(vfqn, _) => Some(vfqn.value)
      case _                                                                 => None
    }
}

object ReconcileProcessor {

  private def langInt(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Int"), QualifiedName(name, Qualifier.Default))

  /** `Bool::fold` — the `if` eliminator; the pass reconciles its two arms to the fold node's joined meta. */
  private[reconcile] val boolFoldFqn: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Bool"), QualifiedName("fold", Qualifier.Default))

  /** The leaves whose codegen consumes operands at their own (per-operand) representation, so the pass must NOT
    * pre-reconcile their operands: the three arithmetic leaves, the ordering leaf, and `intToString`.
    */
  private[reconcile] val widthTransparentLeaves: Set[ValueFQN] =
    Set(
      langInt("nativeAdd"),
      langInt("nativeSubtract"),
      langInt("nativeMultiply"),
      langInt("intLessThanOrEqual"),
      langInt("intToString")
    )

  private val bigIntType: GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type)),
      Seq.empty,
      GroundValue.Type
    )

  private val intervalType: GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(defaultSystemPackage, "Interval"), QualifiedName("Interval", Qualifier.Type)),
      Seq(bigIntType),
      GroundValue.Type
    )

  private val intervalCtorFqn: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Interval"), QualifiedName("Interval", Qualifier.Default))

  /** The channel's per-node interval as the domain value `Interval(lo, hi)` — the generic meta the reconciled fact
    * carries and the backend decodes for width selection.
    */
  private[reconcile] def intervalMeta(min: BigInt, max: BigInt): GroundValue =
    GroundValue.Structure(
      intervalCtorFqn,
      Seq(GroundValue.Direct(min, bigIntType), GroundValue.Direct(max, bigIntType)),
      intervalType
    )

  /** Collapse the channel table to a position-keyed meta map, keeping a position only when every entry at it agrees on
    * one interval (a position with two distinct intervals is dropped, so the node stays ⊤ — matching the lowering
    * pass's `unambiguousIntervalsByPosition`).
    */
  private[reconcile] def metaByPosition(table: RefinementTable): Map[PositionRange, GroundValue] =
    table.intervals
      .groupBy(_.position)
      .collect {
        case (position, entries) if entries.map(ni => (ni.min, ni.max)).distinct.sizeIs == 1 =>
          position -> intervalMeta(entries.head.min, entries.head.max)
      }
}
