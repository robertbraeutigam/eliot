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
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** The refinement channel in **shadow mode** — Step 2a of `docs/bounds-as-refinements.md`.
  *
  * A post-pass over each [[MonomorphicValue]] (runtime track): it walks the fully-ground body and, for every Int-typed
  * node, records the interval the channel knows for it into a [[RefinementTable]]. The point of the pass is the
  * *agreement harness* at arithmetic nodes: at an `Int` `+`/`-`/`*` the result interval is recomputed **independently
  * of the type**, by running the compiler-pool `Interval` arithmetic instance (the future single source of truth, once
  * `Int` loses its type parameters at Step 6) through the one NbE evaluator, and asserted equal to the interval the
  * `Int` associated-type formulas produced (today's source of truth). A divergence is a hard compiler error — the
  * fail-safe the migration wants while the two representations of the same lattice coexist.
  *
  * Why a post-pass and not a rider inside the checker: refinements are, by the design's held invariant, strictly
  * *downstream* of type formation (they flow into checks and codegen, never back into a type), so the channel can run
  * entirely over the checker's output with zero risk to the checker's invariants. See the design doc §3.
  *
  * The arithmetic is recognised at the platform's native leaves (`eliot.lang.Int::nativeAdd`/`nativeSubtract`/
  * `nativeMultiply`) — the point where the transfer is realised, with concrete operand/result types in hand. Those
  * leaves are the JVM layer's names; recognising them here is the channel being legitimately platform-aware (it sits
  * next to codegen). A backend using other leaf names simply gets no shadow check for its arithmetic — reduced
  * coverage, never a false accept, since the type formulas remain the source of truth in shadow mode.
  *
  * Scope of this step: **straight-line transfers only** (2a). Branch-merge joins (`Meta.join`) are 2b.
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
                     case Some(body) => walkNode(body, body.value, mv.signature)
                     case None       => Seq.empty[RefinementTable.NodeInterval].pure[CompilerIO]
                   }
    } yield RefinementTable(key.vfqn, key.typeArguments, intervals)

  /** Walk one body node: record its own interval (if Int-typed), descend into children, and — when the node is an
    * arithmetic transfer application — run the shadow check. `sourced` carries the node's source position (URI + range)
    * for the recorded entry and any diagnostic; `expr`/`tpe` are the node's expression and ground type (threaded
    * separately because the runtime root is a bare `Expression` without its own `expressionType`).
    */
  private def walkNode(
      sourced: Sourced[?],
      expr: MonomorphicExpression.Expression,
      tpe: GroundValue
  ): CompilerIO[Seq[RefinementTable.NodeInterval]] = {
    val selfInterval: Seq[RefinementTable.NodeInterval] =
      intIntervalOf(tpe).map { case (lo, hi) => RefinementTable.NodeInterval(sourced.range, lo, hi) }.toSeq

    expr match {
      case MonomorphicExpression.FunctionApplication(target, argument) =>
        for {
          targetIntervals   <- walkChild(target)
          argumentIntervals <- walkChild(argument)
          shadowIntervals   <- arithmeticTransfer(target, argument) match {
                                 case Some((leaf, operandA, operandB)) =>
                                   shadowCheck(sourced, leaf, operandA, operandB, tpe)
                                 case None                             =>
                                   Seq.empty[RefinementTable.NodeInterval].pure[CompilerIO]
                               }
        } yield selfInterval ++ targetIntervals ++ argumentIntervals ++ shadowIntervals

      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        walkChild(body).map(selfInterval ++ _)

      case _ =>
        selfInterval.pure[CompilerIO]
    }
  }

  private def walkChild(child: Sourced[MonomorphicExpression]): CompilerIO[Seq[RefinementTable.NodeInterval]] =
    walkNode(child, child.value.expression, child.value.expressionType)

  /** Recognise a fully-applied binary arithmetic leaf: an application whose target is itself an application of one of
    * the three native arithmetic leaves to its first operand. Returns the leaf FQN and the two operand nodes.
    */
  private def arithmeticTransfer(
      target: Sourced[MonomorphicExpression],
      argument: Sourced[MonomorphicExpression]
  ): Option[(ValueFQN, Sourced[MonomorphicExpression], Sourced[MonomorphicExpression])] =
    target.value.expression match {
      case MonomorphicExpression.FunctionApplication(innerTarget, operandA) =>
        innerTarget.value.expression match {
          case MonomorphicExpression.MonomorphicValueReference(vfqn, _) if isArithmeticLeaf(vfqn.value) =>
            Some((vfqn.value, operandA, argument))
          case _                                                                                        => None
        }
      case _                                                                => None
    }

  /** The shadow assertion: recompute the transfer's result interval via the compiler-pool `Interval` instance and
    * assert it equals the interval the type formulas produced. Records nothing itself — the result node's interval is
    * already recorded from its type by [[walkNode]]; this only verifies. If the transfer cannot be computed (no
    * instance, abstract operands, a stuck read-back) it stays silent: in shadow mode the type is the source of truth,
    * so a missing channel computation is reduced coverage, never a false accept.
    */
  private def shadowCheck(
      sourced: Sourced[?],
      leaf: ValueFQN,
      operandA: Sourced[MonomorphicExpression],
      operandB: Sourced[MonomorphicExpression],
      resultType: GroundValue
  ): CompilerIO[Seq[RefinementTable.NodeInterval]] =
    (intIntervalOf(operandA.value.expressionType), intIntervalOf(operandB.value.expressionType), intIntervalOf(resultType)) match {
      case (Some(a), Some(b), Some(expected)) =>
        runTransfer(leaf, a, b).flatMap {
          case Some(computed) if computed =!= expected =>
            compilerError(
              sourced.as(
                s"Refinement channel disagrees with the type at an integer ${operatorName(leaf)}: " +
                  s"channel computed [${computed._1}, ${computed._2}] but the type is [${expected._1}, ${expected._2}]."
              ),
              Seq(
                s"Operands: [${a._1}, ${a._2}] ${operatorName(leaf)} [${b._1}, ${b._2}].",
                "This is an internal shadow-mode invariant of the bounds-as-refinements migration; the compiler-pool " +
                  "Interval instance and the Int result-bound formulas must agree on every program."
              )
            ).as(Seq.empty[RefinementTable.NodeInterval])
          case _                                       =>
            Seq.empty[RefinementTable.NodeInterval].pure[CompilerIO]
        }
      case _                                  =>
        Seq.empty[RefinementTable.NodeInterval].pure[CompilerIO]
    }

  /** Compute `[la, ha] op [lb, hb]` by resolving the compiler-pool `Interval` arithmetic instance for the operator and
    * evaluating its reduced body applied to the two interval values through the one NbE evaluator — the
    * `RefinementSolver.combinePair` pattern, re-pointed at the `Interval` transfer. `None` when the instance does not
    * resolve (channel cannot compute — silent in shadow mode) or the result does not read back to an interval.
    */
  private def runTransfer(
      leaf: ValueFQN,
      operandA: (BigInt, BigInt),
      operandB: (BigInt, BigInt)
  ): CompilerIO[Option[(BigInt, BigInt)]] = {
    val abilityMethodFqn = arithmeticAbilityMethod(leaf)
    val intervalValueA   = intervalValue(operandA._1, operandA._2)
    val intervalValueB   = intervalValue(operandB._1, operandB._2)
    for {
      resolved <- getFactIfProduced(
                    AbilityImplementation.Key(abilityMethodFqn, Seq(intervalType, intervalType), Platform.Compiler)
                  ).map(_.flatMap(_.resolution.resolved))
      result   <- resolved match {
                    case None                          => none[(BigInt, BigInt)].pure[CompilerIO]
                    case Some((implFqn, implTypeArgs)) =>
                      ReducedBindingClosure.reduceInstance(implFqn, implTypeArgs).map {
                        case None       => None
                        case Some(body) =>
                          val applied = Evaluator.applyValue(
                            Evaluator.applyValue(body, Evaluator.groundToSem(intervalValueA)),
                            Evaluator.groundToSem(intervalValueB)
                          )
                          val forced  = Evaluator.force(applied, MetaStore.empty)
                          Quoter.quote(0, forced, MetaStore.empty).toOption.flatMap(twoBigIntArgs)
                      }
                  }
    } yield result
  }
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

  /** The interval *value* `Interval(lo, hi)` fed to the transfer. */
  private def intervalValue(lo: BigInt, hi: BigInt): GroundValue =
    GroundValue.Structure(
      intervalCtorFqn,
      Seq(GroundValue.Direct(lo, bigIntType), GroundValue.Direct(hi, bigIntType)),
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
