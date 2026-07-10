package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.domain.MetaStore
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.monomorphize.processor.ReducedBindingClosure
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** The refinement channel in **shadow mode** — Steps 2a (transfers) and 2b (joins) of
  * `docs/bounds-as-refinements.md`.
  *
  * A post-pass over each [[MonomorphicValue]] (runtime track): it walks the fully-ground body and, for every Int-typed
  * node, records the interval the channel knows for it into a [[RefinementTable]]. The point of the pass is the
  * *agreement harness* at the two places the channel *computes* rather than reads:
  *
  *   - **Transfers (2a):** at an `Int` `+`/`-`/`*` the result interval is recomputed by running the compiler-pool
  *     `Interval` arithmetic instance (the future single source of truth, once `Int` loses its type parameters at Step
  *     6) through the one NbE evaluator, and asserted equal to the interval the `Int` associated-type formulas produced.
  *   - **Joins (2b):** at a `match` whose arms carry different `Int` ranges, the merged interval is recomputed by
  *     `Meta.join` on the arms' pre-widening intervals and asserted equal to the interval the type-level `Combine` join
  *     produced (the `handleCases` result type). This is the branch merge where the channel *must* join — a
  *     runtime-chosen result the channel cannot narrow — as opposed to a `pick[A](a: A, b: A): A` covariant "join",
  *     which is a type artifact: at runtime the value is exactly `a`, so the endgame channel is *tighter* than the
  *     type there and the two legitimately diverge (that case dies with `Combine` at Step 6, so it is not checked).
  *
  * A divergence at either is a hard compiler error — the fail-safe the migration wants while the two representations of
  * the same lattice coexist.
  *
  * Why a post-pass and not a rider inside the checker: refinements are, by the design's held invariant, strictly
  * *downstream* of type formation (they flow into checks and codegen, never back into a type), so the channel can run
  * entirely over the checker's output with zero risk to the checker's invariants. See the design doc §3.
  *
  * The arithmetic is recognised at the platform's native leaves (`eliot.lang.Int::nativeAdd`/`nativeSubtract`/
  * `nativeMultiply`); the merge at the pattern-match eliminator (`handleCases`), with the arms' pre-join intervals read
  * off the operands of the `nativeWiden` coercions the type-level join inserted. A backend using other leaf names, or a
  * merge shape this does not recognise, simply gets no shadow check there — reduced coverage, never a false accept,
  * since the type formulas remain the source of truth in shadow mode.
  */
class RefinementChannelProcessor
    extends TransformationProcessor[MonomorphicValue.Key, RefinementTable.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  import RefinementChannelProcessor.*

  override protected def generateFromKeyAndFact(
      key: RefinementTable.Key,
      mv: MonomorphicValue
  ): CompilerIO[RefinementTable] =
    for {
      intervals <- mv.runtime match {
                     case Some(body) => walkNode(body.as(MonomorphicExpression(mv.signature, body.value)))
                     case None       => Seq.empty[RefinementTable.NodeInterval].pure[CompilerIO]
                   }
    } yield RefinementTable(key.vfqn, key.typeArguments, intervals)

  /** Walk one body node: record its own interval (if Int-typed), descend into children (the flattened application head
    * and every argument, or a lambda body), and — when the node is an arithmetic transfer or a pattern-match merge —
    * run the shadow check.
    */
  private def walkNode(node: Sourced[MonomorphicExpression]): CompilerIO[Seq[RefinementTable.NodeInterval]] = {
    val selfInterval: Seq[RefinementTable.NodeInterval] =
      intIntervalOf(node.value.expressionType).map { case (lo, hi) =>
        RefinementTable.NodeInterval(node.range, lo, hi)
      }.toSeq

    node.value.expression match {
      case _: MonomorphicExpression.FunctionApplication =>
        val (head, args) = flatten(node)
        for {
          childIntervals <- (head +: args).flatTraverse(walkNode)
          shadowIntervals <- head.value.expression match {
                               case MonomorphicExpression.MonomorphicValueReference(vfqn, _)
                                   if isArithmeticLeaf(vfqn.value) && args.sizeIs == 2 =>
                                 shadowCheckArithmetic(node, vfqn.value, args(0), args(1))
                               case MonomorphicExpression.MonomorphicValueReference(vfqn, _)
                                   if WellKnownTypes.isPatternMatchHandleCases(vfqn.value) =>
                                 shadowCheckJoin(node, args)
                               case _ =>
                                 Seq.empty[RefinementTable.NodeInterval].pure[CompilerIO]
                             }
        } yield selfInterval ++ childIntervals ++ shadowIntervals

      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        walkNode(body).map(selfInterval ++ _)

      case _ =>
        selfInterval.pure[CompilerIO]
    }
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

  /** The transfer shadow assertion (2a): recompute the result interval via the compiler-pool `Interval` arithmetic
    * instance and assert it equals the interval the type formulas produced (the node's own type). Records nothing —
    * [[walkNode]] already recorded the node's interval from its type; this only verifies. Silent when the transfer
    * cannot be computed (shadow mode: the type stays the source of truth).
    */
  private def shadowCheckArithmetic(
      node: Sourced[MonomorphicExpression],
      leaf: ValueFQN,
      operandA: Sourced[MonomorphicExpression],
      operandB: Sourced[MonomorphicExpression]
  ): CompilerIO[Seq[RefinementTable.NodeInterval]] =
    (intIntervalOf(operandA.value.expressionType), intIntervalOf(operandB.value.expressionType), intIntervalOf(node.value.expressionType)) match {
      case (Some(a), Some(b), Some(expected)) =>
        runTransfer(leaf, a, b).flatMap(assertAgreement(node, s"integer ${operatorName(leaf)}", expected, _))
      case _                                  =>
        Seq.empty[RefinementTable.NodeInterval].pure[CompilerIO]
    }

  /** The join shadow assertion (2b): recompute the merged interval by `Meta.join` on the arms' pre-coercion intervals
    * and assert it equals the merge's result interval (the `handleCases` node's type — the type-level `Combine` join).
    *
    * `match` desugars to `handleCases(scrutinee, cases)`, where `cases` is the Scott encoding of the arms —
    * `Lam($selector -> $selector(arm1)(arm2)…)`, each `armK` a lambda over the constructor's fields whose body is the
    * arm's value, coerced (`nativeWiden`) to the merged range where the arm is narrower. [[matchArmIntervals]] reads
    * *every* arm's pre-coercion interval — narrow arms off the `nativeWiden` operand, an arm already at the merged
    * range off its own type — so the assertion is the genuine join of the arms, not the already-merged values (which
    * would be vacuous, and dropping a widest un-widened arm would false-mismatch). Fires only when the whole `cases`
    * structure parses to ≥2 Int arms; any other shape is silently skipped (fail-safe: no false accept).
    */
  private def shadowCheckJoin(
      node: Sourced[MonomorphicExpression],
      args: Seq[Sourced[MonomorphicExpression]]
  ): CompilerIO[Seq[RefinementTable.NodeInterval]] =
    (intIntervalOf(node.value.expressionType), args.lift(1).flatMap(matchArmIntervals)) match {
      case (Some(expected), Some(arms)) if arms.sizeIs >= 2 =>
        runJoinAll(arms).flatMap(assertAgreement(node, "match join", expected, _))
      case _                                                =>
        Seq.empty[RefinementTable.NodeInterval].pure[CompilerIO]
    }

  /** Parse the `cases` argument of a `handleCases` into the pre-coercion interval of every arm. The structure is
    * `Lam($selector -> $selector(arm1)(arm2)…)`; peel the selector lambda(s), confirm the body is the selector applied
    * to the arm lambdas, and read each arm's interval via [[armBodyInterval]]. `None` if the structure is not this
    * shape or any arm is not an extractable `Int` — either way the join check is skipped rather than risk a false
    * mismatch from a partial arm set.
    */
  private def matchArmIntervals(cases: Sourced[MonomorphicExpression]): Option[Seq[(BigInt, BigInt)]] = {
    val (selector, arms) = flatten(peelLambdas(cases))
    selector.value.expression match {
      case MonomorphicExpression.ParameterReference(_) if arms.nonEmpty =>
        arms.toList.traverse(arm => armBodyInterval(peelLambdas(arm)))
      case _                                                            => None
    }
  }

  /** Compare a channel-computed interval against the expected (type-derived) one, reporting a hard error on divergence.
    * Records nothing; a `None` computation is silent (shadow mode: the type is the source of truth).
    */
  private def assertAgreement(
      node: Sourced[MonomorphicExpression],
      what: String,
      expected: (BigInt, BigInt),
      computed: Option[(BigInt, BigInt)]
  ): CompilerIO[Seq[RefinementTable.NodeInterval]] =
    computed match {
      case Some(result) if result =!= expected =>
        compilerError(
          node.as(
            s"Refinement channel disagrees with the type at a $what: " +
              s"channel computed [${result._1}, ${result._2}] but the type is [${expected._1}, ${expected._2}]."
          ),
          Seq(
            "This is an internal shadow-mode invariant of the bounds-as-refinements migration; the compiler-pool " +
              "Interval channel and the Int type formulas must agree on every program."
          )
        ).as(Seq.empty[RefinementTable.NodeInterval])
      case _                                   =>
        Seq.empty[RefinementTable.NodeInterval].pure[CompilerIO]
    }

  /** The pre-coercion interval of one arm body (its pattern-binding lambdas already peeled): off the `nativeWiden`
    * operand when the arm was widened to the merged range, else the body's own type. `None` when the body is not an
    * extractable `Int`.
    */
  private def armBodyInterval(body: Sourced[MonomorphicExpression]): Option[(BigInt, BigInt)] =
    body.value.expression match {
      case MonomorphicExpression.FunctionApplication(target, operand) if isNativeWiden(target) =>
        intIntervalOf(operand.value.expressionType)
      case _                                                                                   =>
        intIntervalOf(body.value.expressionType)
    }

  private def isNativeWiden(node: Sourced[MonomorphicExpression]): Boolean =
    node.value.expression match {
      case MonomorphicExpression.MonomorphicValueReference(vfqn, _) => vfqn.value == nativeWidenFqn
      case _                                                        => false
    }

  private def peelLambdas(node: Sourced[MonomorphicExpression]): Sourced[MonomorphicExpression] =
    node.value.expression match {
      case MonomorphicExpression.FunctionLiteral(_, _, body) => peelLambdas(body)
      case _                                                 => node
    }

  private def runTransfer(
      leaf: ValueFQN,
      operandA: (BigInt, BigInt),
      operandB: (BigInt, BigInt)
  ): CompilerIO[Option[(BigInt, BigInt)]] =
    applyIntervalInstance(
      arithmeticAbilityMethod(leaf),
      Seq(intervalType, intervalType),
      Seq(intervalValue(operandA), intervalValue(operandB))
    )

  /** Fold a list of arm intervals pairwise through `Meta.join`. `None` if any step does not compute. */
  private def runJoinAll(intervals: Seq[(BigInt, BigInt)]): CompilerIO[Option[(BigInt, BigInt)]] =
    intervals.toList match {
      case Nil          => none[(BigInt, BigInt)].pure[CompilerIO]
      case head :: tail =>
        tail.foldLeftM(Option(head)) {
          case (Some(acc), next) => runJoin(acc, next)
          case (None, _)         => none[(BigInt, BigInt)].pure[CompilerIO]
        }
    }

  private def runJoin(a: (BigInt, BigInt), b: (BigInt, BigInt)): CompilerIO[Option[(BigInt, BigInt)]] =
    applyIntervalInstance(metaJoinFqn, Seq(intervalType), Seq(intervalValue(a), intervalValue(b)))

  /** Resolve a compiler-pool ability instance for the `Interval` domain and evaluate its reduced body applied to the
    * given interval values through the one NbE evaluator — the `RefinementSolver.combinePair` pattern, re-pointed at
    * the `Interval` transfer / `Meta.join`. `None` when the instance does not resolve (channel cannot compute — silent
    * in shadow mode) or the result does not read back to an interval.
    *
    * @param abilityMethodFqn the ability *method* to resolve (`Arithmetic::add`/…, or `Meta::join`)
    * @param abilityTypeArgs  the ability's type arguments (`[Interval, Interval]` for `Arithmetic`, `[Interval]` for `Meta`)
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
}

object RefinementChannelProcessor {
  private val intModule: ModuleName      = ModuleName(ModuleName.defaultSystemPackage, "Int")
  private val arithModule: ModuleName    = ModuleName(ModuleName.defaultSystemPackage, "Arithmetic")
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

  /** The range-widening coercion the type-level join inserts on a `match` arm; the channel reads the arm's pre-join
    * interval off its operand.
    */
  private[channel] val nativeWidenFqn: ValueFQN = ValueFQN(intModule, QualifiedName("nativeWiden", Qualifier.Default))

  /** `Meta.join` — the branch-merge combinator resolved on the `Interval` domain (§4.4). */
  private[channel] val metaJoinFqn: ValueFQN =
    ValueFQN(ModuleName(ModuleName.compilerPackage, "Meta"), QualifiedName("join", Qualifier.Ability("Meta")))

  private[channel] def isArithmeticLeaf(vfqn: ValueFQN): Boolean =
    vfqn == nativeAddFqn || vfqn == nativeSubtractFqn || vfqn == nativeMultiplyFqn

  /** The `Arithmetic` ability method that computes the transfer for a leaf — `add`/`subtract`/`multiply`, in the
    * `Ability("Arithmetic")` namespace. Resolved on the `Interval` operand type to reach the compiler-pool interval
    * instance.
    */
  private[channel] def arithmeticAbilityMethod(leaf: ValueFQN): ValueFQN = {
    val name =
      if (leaf == nativeAddFqn) "add"
      else if (leaf == nativeSubtractFqn) "subtract"
      else "multiply"
    ValueFQN(arithModule, QualifiedName(name, Qualifier.Ability("Arithmetic")))
  }

  private def operatorName(leaf: ValueFQN): String =
    if (leaf == nativeAddFqn) "addition"
    else if (leaf == nativeSubtractFqn) "subtraction"
    else "multiplication"

  /** `Interval[BigInteger, BigInteger]` — the domain type the channel carries for an `Int`'s value range. */
  private val intervalType: GroundValue = GroundValue.Structure(
    ValueFQN(intervalModule, QualifiedName("Interval", Qualifier.Type)),
    Seq(bigIntType, bigIntType),
    GroundValue.Type
  )

  private val intervalCtorFqn: ValueFQN = ValueFQN(intervalModule, QualifiedName("Interval", Qualifier.Default))

  /** The interval *value* `Interval(lo, hi)` fed to an instance. */
  private def intervalValue(bounds: (BigInt, BigInt)): GroundValue =
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
