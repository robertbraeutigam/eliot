package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.MetaStore
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.monomorphize.processor.ReducedBindingClosure
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The refinement channel's **flow analysis** — Step 6-iii of `docs/bounds-as-refinements.md` ("narrow representations
  * from the channel's flow analysis"). Post-flag-day (Step 6-ii) `Int` has lost its type parameters, so a node's value
  * range is no longer in its type; the channel *computes* it by flow and records it into a [[RefinementTable]], keyed by
  * source position, which [[com.vanillasource.eliot.eliotc.monomorphize.lowering.RepresentationLowering]] then reads to
  * pick each `Int`'s machine layout (a narrow wrapper instead of the ⊤/bignum fallback).
  *
  * A post-pass over each [[MonomorphicValue]] (runtime track): it walks the fully-ground body bottom-up and, for every
  * node whose value range it can pin, records that interval. The propagation rules (the value channel of §4):
  *
  *   - **α (literal seeding):** an integer literal `n` seeds the singleton `[n, n]`.
  *   - **Transfers (Step-4c form):** at an `Int` `+`/`-`/`*` leaf the result interval is the leaf's `^Meta` transfer
  *     companion (`rangeAdd^Meta`/… — the base-layer vessels' companions, whose braces spell the transfer as the plain
  *     `intervalAdd`/… endpoint arithmetic bottoming at `Numeric[BigInteger]` natives) evaluated through the one NbE
  *     evaluator on the two operand intervals. Unknown if either operand is unknown.
  *   - **Joins (Meta.join at branches):** at a `fold` (the `if` eliminator) whose two arms carry known `Int` ranges, the
  *     result interval is their `Meta.join`. The arms keep their own (narrower) intervals; codegen reconciles them to
  *     the merge representation at the branch (`ExpressionCodeGenerator.generateBoolIntrinsic`).
  *
  * Everything else is ⊤ (unknown, recorded as no entry, laid out as a bignum) — a parameter, a value reference, a
  * `match` (`handleCases`) result, the body of a lambda, the result of an ordinary call. These are the boundaries of
  * §4/§7 Q4: the flow analysis is intra-procedural, so a value crossing a call/return/field/lambda boundary is ⊤ there
  * (sound: "I know nothing" is always true, just imprecise). The walk still *descends into* the arguments of ordinary
  * calls (so a literal/arithmetic argument narrows and is reconciled to the callee's parameter representation at the
  * call), but never into a lambda body or a branch's arms-as-lambdas (a narrow value returned through a lambda's
  * `apply` bridge would fail its `CHECKCAST` — see the class note on `LambdaGenerator`).
  *
  * Why a post-pass and not a rider inside the checker: refinements are, by the design's held invariant, strictly
  * *downstream* of type formation (they flow into checks and codegen, never back into a type), so the channel can run
  * entirely over the checker's output with zero risk to the checker's invariants. See the design doc §3.
  *
  * The arithmetic is recognised at the platform's native leaves (`eliot.lang.Int::nativeAdd`/`nativeSubtract`/
  * `nativeMultiply`) and the branch at the `Bool::fold` eliminator; a backend using other leaf names simply gets no
  * narrowing there — a bignum layout, sound but wide, never wrong.
  */
class RefinementChannelProcessor
    extends TransformationProcessor[MonomorphicValue.Key, RefinementTable.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  import RefinementChannelProcessor.*

  /** The result of walking one node: the interval the channel knows for the node's *own* value (⊤ = [[None]]), and every
    * per-node interval recorded in the subtree (this node's plus its descendants').
    */
  private type FlowResult = (Option[(BigInt, BigInt)], Seq[RefinementTable.NodeInterval])

  override protected def generateFromKeyAndFact(
      key: RefinementTable.Key,
      mv: MonomorphicValue
  ): CompilerIO[RefinementTable] =
    for {
      result <- mv.runtime match {
                  case Some(body) => walkFlow(body.as(MonomorphicExpression(mv.signature, body.value)))
                  case None       => (none[(BigInt, BigInt)], Seq.empty[RefinementTable.NodeInterval]).pure[CompilerIO]
                }
    } yield RefinementTable(key.vfqn, key.typeArguments, result._2)

  /** Compute one node's flow interval and record it (when known), descending per the propagation rules in the class
    * note. Bottom-up: a node's interval is derived from its children's, and a known interval is recorded at the node's
    * source position for representation lowering to read.
    */
  private def walkFlow(node: Sourced[MonomorphicExpression]): CompilerIO[FlowResult] =
    node.value.expression match {
      case MonomorphicExpression.IntegerLiteral(value)  =>
        // α: an integer literal seeds its singleton interval.
        val interval = (value.value, value.value)
        (Option(interval), Seq(RefinementTable.NodeInterval(node.range, interval._1, interval._2))).pure[CompilerIO]

      case _: MonomorphicExpression.FunctionApplication =>
        val (head, args) = flatten(node)
        head.value.expression match {
          case MonomorphicExpression.MonomorphicValueReference(vfqn, _)
              if isArithmeticLeaf(vfqn.value) && args.sizeIs == 2 =>
            walkArithmetic(node, vfqn.value, args(0), args(1))
          case MonomorphicExpression.MonomorphicValueReference(vfqn, _)
              if vfqn.value == boolFoldFqn && args.sizeIs == 3 =>
            walkBranch(node, args(0), args(1), args(2))
          case _ =>
            // Any other application (an ordinary call, a constructor, a `match`, a `typeMatch`, an applied lambda): the
            // result is a ⊤ boundary, but descend into the arguments so a literal/arithmetic argument still narrows and
            // is reconciled to the callee's parameter representation at the call. A lambda argument's body is skipped by
            // the FunctionLiteral case below.
            args.flatTraverse(walkFlow(_).map(_._2)).map(records => (none[(BigInt, BigInt)], records))
        }

      case MonomorphicExpression.FunctionLiteral(_, _, _) =>
        // Do not descend into a lambda body: its `apply` bridge would `CHECKCAST` a narrowed result back to the ⊤/bignum
        // representation the caller expects, so a lambda body stays a bignum boundary (Step 6-iii).
        (none[(BigInt, BigInt)], Seq.empty[RefinementTable.NodeInterval]).pure[CompilerIO]

      case _ =>
        // A parameter/value reference or a string literal: ⊤ (no known integer range at this node).
        (none[(BigInt, BigInt)], Seq.empty[RefinementTable.NodeInterval]).pure[CompilerIO]
    }

  /** An arithmetic leaf node: recurse into the two operands, then compute the result interval via the leaf's `^Meta`
    * transfer companion when both operands are known (else ⊤).
    */
  private def walkArithmetic(
      node: Sourced[MonomorphicExpression],
      leaf: ValueFQN,
      operandA: Sourced[MonomorphicExpression],
      operandB: Sourced[MonomorphicExpression]
  ): CompilerIO[FlowResult] =
    for {
      (aInterval, aRecords) <- walkFlow(operandA)
      (bInterval, bRecords) <- walkFlow(operandB)
      result                <- (aInterval, bInterval) match {
                                 case (Some(a), Some(b)) => runTransfer(leaf, a, b)
                                 case _                  => none[(BigInt, BigInt)].pure[CompilerIO]
                               }
    } yield (result, aRecords ++ bRecords ++ recordAt(node, result))

  /** A `fold` branch node: recurse into the condition (so arithmetic inside it narrows against the tolerant `Compare`
    * leaf) and both arms, then compute the result interval as the `Meta.join` of the arms when both are known (else ⊤).
    * The arms keep their own (narrower) intervals; the merge representation is reconciled at codegen.
    */
  private def walkBranch(
      node: Sourced[MonomorphicExpression],
      condition: Sourced[MonomorphicExpression],
      whenTrue: Sourced[MonomorphicExpression],
      whenFalse: Sourced[MonomorphicExpression]
  ): CompilerIO[FlowResult] =
    for {
      (_, condRecords)          <- walkFlow(condition)
      (trueInterval, trueRecs)  <- walkFlow(whenTrue)
      (falseInterval, falseRecs) <- walkFlow(whenFalse)
      result                    <- (trueInterval, falseInterval) match {
                                     case (Some(t), Some(f)) => runJoin(t, f)
                                     case _                  => none[(BigInt, BigInt)].pure[CompilerIO]
                                   }
    } yield (result, condRecords ++ trueRecs ++ falseRecs ++ recordAt(node, result))

  private def recordAt(
      node: Sourced[MonomorphicExpression],
      interval: Option[(BigInt, BigInt)]
  ): Seq[RefinementTable.NodeInterval] =
    interval.map { case (lo, hi) => RefinementTable.NodeInterval(node.range, lo, hi) }.toSeq

  /** Flatten a curried application into its ultimate head and its arguments in source order. */
  private def flatten(
      node: Sourced[MonomorphicExpression]
  ): (Sourced[MonomorphicExpression], Seq[Sourced[MonomorphicExpression]]) =
    node.value.expression match {
      case MonomorphicExpression.FunctionApplication(target, argument) =>
        val (head, args) = flatten(target)
        (head, args :+ argument)
      case _                                                           => (node, Seq.empty)
    }

  private def runJoin(a: (BigInt, BigInt), b: (BigInt, BigInt)): CompilerIO[Option[(BigInt, BigInt)]] =
    applyIntervalInstance(metaJoinFqn, Seq(intervalType), Seq(intervalValue(a), intervalValue(b)))

  /** Resolve a compiler-pool ability instance for the `Interval` domain and evaluate its reduced body applied to the
    * given interval values through the one NbE evaluator — the `RefinementSolver.combinePair` pattern, re-pointed at
    * `Meta.join`. `None` when the instance does not resolve (channel cannot compute) or the result does not read back to
    * an interval.
    *
    * @param abilityMethodFqn the ability *method* to resolve (`Meta::join`)
    * @param abilityTypeArgs  the ability's type arguments (`[Interval]` for the single-parameter `Meta`)
    * @param intervalValues   the value arguments to apply (each an `Interval(lo, hi)` ground value)
    */
  private def applyIntervalInstance(
      abilityMethodFqn: ValueFQN,
      abilityTypeArgs: Seq[GroundValue],
      intervalValues: Seq[GroundValue]
  ): CompilerIO[Option[(BigInt, BigInt)]] =
    for {
      resolved <- getFactIfProduced(AbilityImplementation.Key(abilityMethodFqn, abilityTypeArgs, Platform.Compiler))
                    .map(_.flatMap(_.resolution.resolved))
      result   <- resolved match {
                    case None                          => none[(BigInt, BigInt)].pure[CompilerIO]
                    case Some((implFqn, implTypeArgs)) =>
                      ReducedBindingClosure.reduceInstance(implFqn, implTypeArgs).map {
                        case None       => None
                        case Some(body) =>
                          val applied = intervalValues.foldLeft(body) { (f, iv) =>
                            Evaluator.applyValue(f, Evaluator.groundToSem(iv))
                          }
                          val forced  = Evaluator.force(applied, MetaStore.empty)
                          Quoter.quote(0, forced, MetaStore.empty).toOption.flatMap(twoBigIntArgs)
                      }
                  }
    } yield result

  /** Recompute an arithmetic transfer by evaluating the leaf's `^Meta` transfer companion (`docs/bounds-as-refinements.md`
    * Step 4c). `nativeAdd`/… maps to `rangeAdd^Meta`/… (the base-layer vessels' companions); the companion is applied to
    * the two operand ranges wrapped as `Int$Meta(Interval(lo,hi))` and its result `Int$Meta`'s `range` slot is read
    * back. This replaces Step 5's direct resolution of the `Numeric[Interval]` instance — the transfer now lives in the
    * vessel's return brace (Eliot), evaluated via the uniform `^Meta` mechanism.
    */
  private def runTransfer(
      leaf: ValueFQN,
      operandA: (BigInt, BigInt),
      operandB: (BigInt, BigInt)
  ): CompilerIO[Option[(BigInt, BigInt)]] =
    applyMetaCompanion(
      metaTransferCompanion(leaf),
      Seq(intMetaValue(operandA), intMetaValue(operandB))
    )

  /** Reduce a compiler-pool `^Meta` transfer companion (non-generic, so empty type args), apply it to the operand meta
    * values through the one NbE evaluator, and read the result `Int$Meta`'s single `range` slot back as an interval —
    * `ReducedBindingClosure.reduceInstance` + `Evaluator.applyValue`/`force` + `Quoter.quote`, the same pattern
    * [[applyIntervalInstance]] uses for the join, re-pointed from an ability instance to the `^Meta` companion. `None`
    * when the companion does not reduce (channel cannot compute — silent in shadow mode) or the result is not the
    * expected `Int$Meta(Interval(...))` shape.
    */
  private def applyMetaCompanion(
      companionFqn: ValueFQN,
      metaValues: Seq[GroundValue]
  ): CompilerIO[Option[(BigInt, BigInt)]] =
    ReducedBindingClosure.reduceInstance(companionFqn, Seq.empty).map {
      case None       => None
      case Some(body) =>
        val applied = metaValues.foldLeft(body) { (f, mv) =>
          Evaluator.applyValue(f, Evaluator.groundToSem(mv))
        }
        val forced  = Evaluator.force(applied, MetaStore.empty)
        Quoter.quote(0, forced, MetaStore.empty).toOption.flatMap(unwrapIntMeta).flatMap(twoBigIntArgs)
    }
}

object RefinementChannelProcessor {
  private val intModule: ModuleName      = ModuleName(ModuleName.defaultSystemPackage, "Int")
  private val intervalModule: ModuleName = ModuleName(ModuleName.defaultSystemPackage, "Interval")
  private val bigIntModule: ModuleName   = ModuleName(ModuleName.defaultSystemPackage, "BigInteger")

  /** The `Int` type constructor FQN — `eliot.lang.Int::Int` (type namespace). The compiler otherwise never names
    * `Int`; the channel is a deliberate, downstream-of-types exception (representation is its business).
    */
  private[channel] val intTypeFqn: ValueFQN = ValueFQN(intModule, QualifiedName("Int", Qualifier.Type))

  private val bigIntType: GroundValue = GroundValue.Structure(
    ValueFQN(bigIntModule, QualifiedName("BigInteger", Qualifier.Type)),
    Seq.empty,
    GroundValue.Type
  )

  /** The three platform arithmetic leaves the transfer is recognised at (JVM layer names — see the class note). */
  private[channel] val nativeAddFqn: ValueFQN      = ValueFQN(intModule, QualifiedName("nativeAdd", Qualifier.Default))
  private[channel] val nativeSubtractFqn: ValueFQN = ValueFQN(intModule, QualifiedName("nativeSubtract", Qualifier.Default))
  private[channel] val nativeMultiplyFqn: ValueFQN = ValueFQN(intModule, QualifiedName("nativeMultiply", Qualifier.Default))

  /** `Bool::fold` — the `if` eliminator; the channel joins its two arms' intervals into the branch result (§4.4). */
  private[channel] val boolFoldFqn: ValueFQN =
    ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Bool"), QualifiedName("fold", Qualifier.Default))

  /** `Meta.join` — the branch-merge combinator resolved on the `Interval` domain (§4.4). */
  private[channel] val metaJoinFqn: ValueFQN =
    ValueFQN(ModuleName(ModuleName.compilerPackage, "Meta"), QualifiedName("join", Qualifier.Ability("Meta")))

  private[channel] def isArithmeticLeaf(vfqn: ValueFQN): Boolean =
    vfqn == nativeAddFqn || vfqn == nativeSubtractFqn || vfqn == nativeMultiplyFqn

  /** The `^Meta` transfer companion that computes the transfer for a leaf — `rangeAdd`/`rangeSubtract`/`rangeMultiply`,
    * in the [[Qualifier.Meta]] namespace (`docs/bounds-as-refinements.md` Step 4c). These are the base-layer vessels'
    * companions (`stdlib/.../Int.els`), generated by `MetaTransferDesugarer` from each vessel's return brace, which the
    * channel reduces and evaluates to propagate an `Int`'s range through arithmetic.
    */
  private[channel] def metaTransferCompanion(leaf: ValueFQN): ValueFQN =
    if (leaf == nativeAddFqn) rangeAddMetaFqn
    else if (leaf == nativeSubtractFqn) rangeSubtractMetaFqn
    else rangeMultiplyMetaFqn

  private[channel] val rangeAddMetaFqn: ValueFQN      = ValueFQN(intModule, QualifiedName("rangeAdd", Qualifier.Meta))
  private[channel] val rangeSubtractMetaFqn: ValueFQN = ValueFQN(intModule, QualifiedName("rangeSubtract", Qualifier.Meta))
  private[channel] val rangeMultiplyMetaFqn: ValueFQN = ValueFQN(intModule, QualifiedName("rangeMultiply", Qualifier.Meta))

  /** `Int$Meta` — the meta structure `MetaConstructorDesugarer` emits for `type Int {range: …}` (the `$Meta` suffix is
    * `MetaConstructorDesugarer.metaTypeSuffix`). Its value constructor takes the sole `range` slot (an `Interval`).
    */
  private val intMetaType: GroundValue = GroundValue.Structure(
    ValueFQN(intModule, QualifiedName("Int$Meta", Qualifier.Type)),
    Seq.empty,
    GroundValue.Type
  )
  private val intMetaCtorFqn: ValueFQN = ValueFQN(intModule, QualifiedName("Int$Meta", Qualifier.Default))

  /** An operand's range wrapped as the `Int$Meta(Interval(lo, hi))` value the `^Meta` companion consumes. */
  private[channel] def intMetaValue(bounds: (BigInt, BigInt)): GroundValue =
    GroundValue.Structure(intMetaCtorFqn, Seq(intervalValue(bounds)), intMetaType)

  /** Peel one `Int$Meta(interval)` structure level to its sole `range`-slot argument (the result `Interval`), so the
    * endpoints can be read by [[twoBigIntArgs]]. `None` for any other shape.
    */
  private def unwrapIntMeta(gv: GroundValue): Option[GroundValue] = gv match {
    case GroundValue.Structure(_, Seq(interval), _) => Some(interval)
    case _                                          => None
  }

  /** `Interval[BigInteger]` — the domain type the channel carries for an `Int`'s value range. Its value constructor
    * `Interval(start, end)` still takes two endpoint fields; only the type constructor is single-parameter now.
    */
  private[channel] val intervalType: GroundValue = GroundValue.Structure(
    ValueFQN(intervalModule, QualifiedName("Interval", Qualifier.Type)),
    Seq(bigIntType),
    GroundValue.Type
  )

  private val intervalCtorFqn: ValueFQN = ValueFQN(intervalModule, QualifiedName("Interval", Qualifier.Default))

  /** The interval *value* `Interval(lo, hi)` fed to an instance. */
  private[channel] def intervalValue(bounds: (BigInt, BigInt)): GroundValue =
    GroundValue.Structure(
      intervalCtorFqn,
      Seq(GroundValue.Direct(bounds._1, bigIntType), GroundValue.Direct(bounds._2, bigIntType)),
      intervalType
    )

  /** The `[min, max]` of an `Int[min, max]` ground type, or `None` for any other type. */
  private[channel] def intIntervalOf(gv: GroundValue): Option[(BigInt, BigInt)] = gv match {
    case structure @ GroundValue.Structure(fqn, _, _) if fqn == intTypeFqn => twoBigIntArgs(structure)
    case _                                                                 => None
  }

  /** Extract two `Direct` big-integer arguments from a structure — used for both an `Int[min, max]` type and an
    * `Interval(lo, hi)` value (both carry exactly two integer endpoints).
    */
  private def twoBigIntArgs(gv: GroundValue): Option[(BigInt, BigInt)] = gv match {
    case GroundValue.Structure(_, Seq(a, b), _) =>
      (directBigInt(a), directBigInt(b)).tupled
    case _                                      => None
  }

  private def directBigInt(gv: GroundValue): Option[BigInt] = gv match {
    case GroundValue.Direct(value, _) =>
      value match {
        case b: BigInt               => Some(b)
        case b: java.math.BigInteger => Some(BigInt(b))
        case i: Int                  => Some(BigInt(i))
        case l: Long                 => Some(BigInt(l))
        case _                       => None
      }
    case _                            => None
  }
}
