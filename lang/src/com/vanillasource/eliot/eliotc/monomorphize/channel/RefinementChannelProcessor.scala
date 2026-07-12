package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.processor.{MetaConstructorDesugarer, MetaWhereDesugarer}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, UnifiedModuleNames, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.domain.{MetaStore, SemValue}
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
  * source position. The reconcile pass ([[com.vanillasource.eliot.eliotc.reconcile.processor.ReconcileProcessor]]) then
  * stamps those intervals onto the body as per-node metas, and the JVM backend decodes each `Int`'s machine layout from
  * its meta (a narrow wrapper instead of the ⊤/bignum fallback).
  *
  * A post-pass over each [[MonomorphicValue]] (runtime track): it walks the fully-ground body bottom-up and, for every
  * node whose value range it can pin, records that interval. The propagation rules (the value channel of §4):
  *
  *   - **α (literal seeding):** an integer literal `n` seeds the singleton `[n, n]`.
  *   - **Transfers (Step-4c form):** at an `Int` `+`/`-`/`*` leaf the result interval is the leaf's `^Meta` transfer
  *     companion (`rangeAdd^Meta`/… — the base-layer vessels' companions, whose braces spell the transfer as the plain
  *     `intervalAdd`/… endpoint arithmetic bottoming at `Numeric[BigInteger]` natives) evaluated through the one NbE
  *     evaluator on the two operand intervals. Unknown if either operand is unknown.
  *   - **Merges (Step 3):** at *any ordinary call* whose callee declares a `^Meta` **merge** companion — e.g. `fold`,
  *     whose `fold^Meta` spells `join(whenTrue, whenFalse)` over the domain's `Meta.join` — the result interval is that
  *     companion reduced on the argument metas (`mergeViaCompanion`), *mechanically identical* to a transfer: no branch
  *     construct is ever named. So `fold` narrows through the same generic path as any range-moving native, and any
  *     future selector merges for free. The arms keep their own (narrower) intervals; the reconcile pass re-encodes
  *     each to the merged representation at the branch (`docs/generic-refinement-merges.md` §1).
  *
  * Everything else is ⊤ (unknown, recorded as no entry, laid out as a bignum) — a parameter, a value reference, a
  * `match` (`handleCases`) result, the body of a lambda, an ordinary call with no `^Meta` companion. These are the
  * boundaries of §4/§7 Q4: the flow analysis is intra-procedural, so a value crossing a call/return/field/lambda
  * boundary is ⊤ there (sound: "I know nothing" is always true, just imprecise). The walk still *descends into* the
  * arguments of ordinary calls (so a literal/arithmetic argument narrows and is reconciled to the callee's parameter
  * representation at the call), but never into a lambda body or a branch's arms-as-lambdas (a narrow value returned
  * through a lambda's `apply` bridge would fail its `CHECKCAST` — see the class note on `LambdaGenerator`).
  *
  * Why a post-pass and not a rider inside the checker: refinements are, by the design's held invariant, strictly
  * *downstream* of type formation (they flow into checks and codegen, never back into a type), so the channel can run
  * entirely over the checker's output with zero risk to the checker's invariants. See the design doc §3.
  *
  * The arithmetic is recognised at the platform's native leaves (`eliot.lang.Int::nativeAdd`/`nativeSubtract`/
  * `nativeMultiply`); a merge is recognised by the callee's `^Meta` companion, never by naming a branch construct
  * (`docs/generic-refinement-merges.md`). A backend using other leaf names simply gets no narrowing there — a bignum
  * layout, sound but wide, never wrong.
  */
class RefinementChannelProcessor
    extends TransformationProcessor[MonomorphicValue.Key, RefinementTable.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  import RefinementChannelProcessor.*

  /** The result of walking one node: the interval the channel knows for the node's *own* value (⊤ = [[None]]) and every
    * per-node interval recorded in the subtree (this node's plus its descendants').
    */
  private case class Flow(
      own: Option[(BigInt, BigInt)],
      records: Seq[RefinementTable.NodeInterval]
  )

  private object Flow {
    val topBoundary: Flow = Flow(None, Seq.empty)
  }

  override protected def generateFromKeyAndFact(
      key: RefinementTable.Key,
      mv: MonomorphicValue
  ): CompilerIO[RefinementTable] =
    for {
      result <- mv.runtime match {
                  case Some(body) => walkFlow(body.as(MonomorphicExpression(mv.signature, body.value)))
                  case None       => Flow.topBoundary.pure[CompilerIO]
                }
    } yield RefinementTable(key.vfqn, key.typeArguments, result.records)

  /** Compute one node's flow interval and record it (when known), descending per the propagation rules in the class
    * note. Bottom-up: a node's interval is derived from its children's, and a known interval is recorded at the node's
    * source position for representation lowering to read.
    */
  private def walkFlow(node: Sourced[MonomorphicExpression]): CompilerIO[Flow] =
    node.value.expression match {
      case MonomorphicExpression.IntegerLiteral(value)  =>
        // α: an integer literal seeds its singleton interval.
        val interval = (value.value, value.value)
        Flow(Option(interval), Seq(RefinementTable.NodeInterval(node.range, interval._1, interval._2))).pure[CompilerIO]

      case _: MonomorphicExpression.FunctionApplication =>
        val (head, args) = flatten(node)
        head.value.expression match {
          case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) =>
            // An ordinary call (or constructor). Descend into the arguments (so a literal/arithmetic argument narrows,
            // and a `where` precondition is demanded over their ranges, bounds-as-refinements §4.3). The result is a ⊤
            // boundary **unless** the callee declares a `^Meta` companion — a **transfer** (`nativeAdd`, whose result
            // range is `intervalAdd` of the operand ranges) or a **merge** (`fold`, whose result range is `Meta.join`
            // of its arms). Both are computed by the *same* uniform path: reduce `<callee>^Meta` on the argument metas
            // (`metaViaCompanion`). The channel names no leaf and no branch construct — the `^Meta` companion is the one
            // recognition point (`docs/generic-refinement-merges.md`). A lambda argument's body is skipped by the
            // `FunctionLiteral` case below.
            for {
              argResults <- args.traverse(walkFlow)
              _          <- checkWhere(node, vfqn.value, typeArgs, args.size, argResults.map(_.own))
              merge      <- metaViaCompanion(vfqn.value, typeArgs, argResults.map(_.own))
            } yield Flow(merge, argResults.flatMap(_.records) ++ recordAt(node, merge))
          case _ =>
            // Any other application (a `match`, a `typeMatch`, an applied lambda): the result is a ⊤ boundary; descend
            // into the arguments as above.
            args.traverse(walkFlow).map { rs =>
              Flow(None, rs.flatMap(_.records))
            }
        }

      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        // A lambda body must not *record* narrow intervals: its `apply` bridge would `CHECKCAST` a narrowed result back
        // to the ⊤/bignum representation the caller expects, so it stays a bignum boundary for representation (Step
        // 6-iii). But it must still be *walked*, so a `where` precondition on a call inside it is demanded (a def's own
        // parameters make its body a leading lambda, so without this every call in a parametered def would escape the
        // check — the §4.3 use-site verification must not have that hole). The records are discarded; only `checkWhere`'s
        // effects during the walk remain.
        walkFlow(body).as(Flow.topBoundary)

      case _ =>
        // A parameter/value reference or a string literal: ⊤ (no known integer range at this node).
        Flow.topBoundary.pure[CompilerIO]
    }

  /** The refinement result of a call, when its callee declares a `^Meta` companion — a **transfer** (`nativeAdd^Meta`,
    * whose result range is `intervalAdd` of the operand ranges) or a **merge** (`fold^Meta`, whose result range is
    * `Meta.join` of its arms). Both are computed uniformly: reduce `<callee>^Meta` on the arguments' metas and read its
    * result `Int$Meta`'s `range` slot back. `None` (⊤) when the callee has no companion, or an input's range is unknown.
    *
    * The companion is reduced at the **meta** type arguments — the call's base type args mapped through [[metaTypeOf]].
    * A *monomorphic* companion (`nativeAdd`, no type args) reduces at `[]` and its `Int$Meta` params take the operand
    * metas directly. A *generic* companion (`fold[A]`) reduces at `A := metaTypeOf(Int) = Int$Meta`, so the bare `A`
    * params bind to the meta type; its `join` then dispatches via the compiler-derived `Meta[Int$Meta]` to
    * `Int$Meta(intervalJoin(range(whenTrue), range(whenFalse)))`. An unknown/non-`Int` argument (⊤ — e.g. `fold`'s
    * `condition`) is a `VType` placeholder the reduction ignores unless it feeds a slot projection, in which case the
    * projection stalls and the result is ⊤ (sound). A merge over untracked arms reduces at `A := Unit` (an untracked
    * type's [[metaTypeOf]] is `Unit`) through the trivial `Meta[Unit]` and reads back no interval (⊤). The membership
    * test (a cached [[UnifiedModuleNames]] lookup for the companion name) keeps a companion-free call to one cheap lookup.
    */
  private def metaViaCompanion(
      callee: ValueFQN,
      calleeTypeArgs: Seq[GroundValue],
      argIntervals: Seq[Option[(BigInt, BigInt)]]
  ): CompilerIO[Option[(BigInt, BigInt)]] =
    getFactIfProduced(UnifiedModuleNames.Key(callee.moduleName, Platform.Compiler)).flatMap { namesOpt =>
      if (!namesOpt.exists(_.names.contains(QualifiedName(callee.name.name, Qualifier.Meta))))
        none[(BigInt, BigInt)].pure[CompilerIO]
      else
        for {
          metaArgs <- calleeTypeArgs.traverse(metaTypeOf)
          reduced  <- ReducedBindingClosure.reduceInstance(metaCompanionFqn(callee), metaArgs)
        } yield reduced.flatMap { body =>
          val argMetas = argIntervals.map {
            case Some(iv) => Evaluator.groundToSem(intMetaValue(iv))
            case None     => SemValue.VType // ⊤ placeholder: an unknown/non-Int argument (e.g. `fold`'s condition)
          }
          val applied = argMetas.foldLeft(body)((f, mv) => Evaluator.applyValue(f, mv))
          val forced  = Evaluator.force(applied, MetaStore.empty)
          Quoter.quote(0, forced, MetaStore.empty).toOption.flatMap(unwrapIntMeta).flatMap(twoBigIntArgs)
        }
    }

  /** The **meta type** of a base type — its `$Meta` meta structure if the type declares one (a slotted type like `Int`
    * ⤳ `Int$Meta`), else the trivial [[unitType]] (any untracked type carries no refinement, so its meta is `Unit`).
    * This is the total-meta rule the deleted `metaOf` intrinsic used to approximate: a generic `^Meta` companion is
    * reduced at these, binding a bare type-parameter param straight to the meta type, and it *always* lands on a real
    * `Meta` instance (`Meta[Int$Meta]` or `Meta[Unit]`) — never a stuck non-existent `T$Meta`. The membership test is a
    * cached [[UnifiedModuleNames]] lookup in the base type's own module. A non-structure type argument is left unchanged.
    */
  private def metaTypeOf(baseType: GroundValue): CompilerIO[GroundValue] = baseType match {
    case GroundValue.Structure(fqn, _, _) =>
      val metaName = QualifiedName(fqn.name.name + MetaConstructorDesugarer.metaTypeSuffix, Qualifier.Type)
      getFactIfProduced(UnifiedModuleNames.Key(fqn.moduleName, Platform.Compiler)).map { namesOpt =>
        if (namesOpt.exists(_.names.contains(metaName)))
          GroundValue.Structure(ValueFQN(fqn.moduleName, metaName), Seq.empty, GroundValue.Type)
        else unitType
      }
    case other                            => other.pure[CompilerIO]
  }

  private def recordAt(
      node: Sourced[MonomorphicExpression],
      interval: Option[(BigInt, BigInt)]
  ): Seq[RefinementTable.NodeInterval] =
    interval.map { case (lo, hi) => RefinementTable.NodeInterval(node.range, lo, hi) }.toSeq

  /** Demand a callee's `where` precondition (bounds-as-refinements §4.3) at this call site, when it declares one. A def
    * `def f(x: Int): T where within(0, 255, range(x))` desugars to a `^Where` companion `f$Where(x: Int$Meta): Bool =
    * within(0, 255, range(x))` ([[MetaWhereDesugarer]]); at a *full* call to `f` that companion is reduced on the
    * compiler track and evaluated over the arguments' channel intervals. The demand is discharged only when every
    * argument's range is known and the predicate reduces to `true`; an unknown (⊤) argument range or a `false` result is
    * a hard error at the call — the use-site verification the cornerstone prescribes. A partial application is not yet a
    * call (left for the full application); a callee that declares no companion has no `where`. The presence of a
    * companion is a cheap per-module [[UnifiedModuleNames]] membership test, so an ordinary call to a `where`-free callee
    * costs one cached lookup and never demands a non-existent companion fact.
    */
  private def checkWhere(
      callNode: Sourced[MonomorphicExpression],
      callee: ValueFQN,
      calleeTypeArgs: Seq[GroundValue],
      appliedArgs: Int,
      argIntervals: Seq[Option[(BigInt, BigInt)]]
  ): CompilerIO[Unit] =
    getFactIfProduced(UnifiedModuleNames.Key(callee.moduleName, Platform.Compiler)).flatMap { namesOpt =>
      if (!namesOpt.exists(_.names.contains(whereCompanionName(callee)))) ().pure[CompilerIO]
      else
        (
          ReducedBindingClosure.reduceInstance(whereCompanionFqn(callee), Seq.empty),
          getFactIfProduced(MonomorphicValue.Key(callee, calleeTypeArgs))
        ).tupled.flatMap {
          case (Some(companion), Some(mv)) =>
            mv.naturalArity match {
              case Some(arity) if arity > 0 && appliedArgs >= arity =>
                demandPrecondition(callNode, callee, companion, argIntervals.take(arity))
              case _                                                => ().pure[CompilerIO]
            }
          case _                           => ().pure[CompilerIO]
        }
    }

  /** Evaluate a resolved `^Where` companion over the call's argument intervals and turn the verdict into a use-site
    * error or a pass. Every argument range must be known (⊤ cannot discharge a demand — the fail-safe of §4.3); the
    * predicate then reduces (through the one NbE evaluator, over the arguments wrapped as `Int$Meta(Interval(lo,hi))`)
    * to a `Bool`: `true` passes, `false` is a violation, and a non-`Bool` result (an unsupported predicate shape) fails
    * loudly rather than silently accepting.
    */
  private def demandPrecondition(
      callNode: Sourced[MonomorphicExpression],
      callee: ValueFQN,
      companion: SemValue,
      argIntervals: Seq[Option[(BigInt, BigInt)]]
  ): CompilerIO[Unit] =
    argIntervals.sequence match {
      case None            =>
        Sourced.compilerError(
          callNode.as(s"Cannot prove the precondition of '${callee.show}': an argument's value range is not known here."),
          Seq("A `where` precondition demands a provable range — pass a value whose range the compiler can determine.")
        )
      case Some(intervals) =>
        val applied = intervals.foldLeft(companion) { (f, iv) =>
          Evaluator.applyValue(f, Evaluator.groundToSem(intMetaValue(iv)))
        }
        Quoter.quote(0, Evaluator.force(applied, MetaStore.empty), MetaStore.empty).toOption match {
          case Some(gv) if isBoolTrue(gv)  => ().pure[CompilerIO]
          case Some(gv) if isBoolFalse(gv) =>
            Sourced.compilerError(
              callNode.as(s"The precondition of '${callee.show}' is not satisfied by the argument's value range.")
            )
          case _                           =>
            Sourced.compilerError(callNode.as(s"Cannot evaluate the `where` precondition of '${callee.show}'."))
        }
    }

  private def isBoolTrue(gv: GroundValue): Boolean = gv match {
    case GroundValue.Direct(true, _)      => true
    case GroundValue.Structure(fqn, _, _) => fqn == WellKnownTypes.boolTrueFQN
    case _                                => false
  }

  private def isBoolFalse(gv: GroundValue): Boolean = gv match {
    case GroundValue.Direct(false, _)     => true
    case GroundValue.Structure(fqn, _, _) => fqn == WellKnownTypes.boolFalseFQN
    case _                                => false
  }

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

}

object RefinementChannelProcessor {
  private val intModule: ModuleName      = ModuleName(ModuleName.defaultSystemPackage, "Int")
  private val intervalModule: ModuleName = ModuleName(ModuleName.defaultSystemPackage, "Interval")
  private val bigIntModule: ModuleName   = ModuleName(ModuleName.defaultSystemPackage, "BigInteger")

  private val bigIntType: GroundValue = GroundValue.Structure(
    ValueFQN(bigIntModule, QualifiedName("BigInteger", Qualifier.Type)),
    Seq.empty,
    GroundValue.Type
  )

  /** A callee's `^Meta` companion FQN: its own name in the [[Qualifier.Meta]] namespace, same module — what
    * [[MetaTransferDesugarer]] emits from a return brace. `fold` ⤳ `fold^Meta` (merge), `nativeAdd` ⤳ `nativeAdd^Meta`
    * (transfer). The channel recognises a refinement operation *only* by the presence of this companion — no leaf or
    * branch construct is named.
    */
  private[channel] def metaCompanionFqn(callee: ValueFQN): ValueFQN =
    ValueFQN(callee.moduleName, QualifiedName(callee.name.name, Qualifier.Meta))

  private val unitModule: ModuleName = ModuleName(ModuleName.defaultSystemPackage, "Unit")

  /** `Unit` — the meta type of every untracked (slotless) type. Its trivial `Meta[Unit]` instance (declared with `Unit`)
    * is the do-nothing join, so a merge over untracked arms reduces cleanly to no refinement (⊤).
    */
  private[channel] val unitType: GroundValue =
    GroundValue.Structure(ValueFQN(unitModule, QualifiedName("Unit", Qualifier.Type)), Seq.empty, GroundValue.Type)

  /** The name / FQN of a def's `^Where` companion (bounds-as-refinements §4.3): the def's own name suffixed with
    * [[MetaWhereDesugarer.whereSuffix]], in the [[Qualifier.Meta]] namespace and the def's own module — exactly what
    * [[MetaWhereDesugarer]] emits, so the channel finds the precondition companion `MetaWhereDesugarer` generated.
    */
  private[channel] def whereCompanionName(callee: ValueFQN): QualifiedName =
    QualifiedName(callee.name.name + MetaWhereDesugarer.whereSuffix, Qualifier.Meta)

  private[channel] def whereCompanionFqn(callee: ValueFQN): ValueFQN =
    ValueFQN(callee.moduleName, whereCompanionName(callee))

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

  /** Extract two `Direct` big-integer arguments from a structure — an `Interval(lo, hi)` value or an `Int$Meta`'s
    * unwrapped range (both carry exactly two integer endpoints).
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
