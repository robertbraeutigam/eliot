package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.MonomorphicEvaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue, MonomorphicExpression, NativeBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

/** Turns the compiler backend's *reduced* body (a [[MonomorphicExpression]] from
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.CompilerMonomorphicValue]]) into a self-contained [[NativeBinding]]
  * value (a `VTopDef` carrying a lazy thunk), closing the reduced body over its dependencies' [[NativeBinding]]s. The
  * [[MonomorphicExpression]] analogue of [[BindingClosure]] — same shape (a lazy self-contained thunk over one-hop
  * dependency bindings, mutual-recursion-guarded via the active fact-request chain), over the reduced compile-time tree
  * instead of the raw operator-resolved body.
  *
  * Used by [[CompilerNativesProcessor]] for a compiler-platform value whose body performs ability dispatch: its raw body
  * cannot reduce against the abstract ability method (`raise`/`pure` have no binding), so its `compiler`-label
  * contribution is this *reduced* form (the concrete `Either::raise`/`Either::pure`), which both the compiler track (when
  * it inlines the value into another compile-time reduction) and the runtime track (when it evaluates the value in a type
  * position) can then reduce.
  */
object ReducedBindingClosure {

  /** Reduce `vfqn` at concrete `typeArguments` on the **compiler track** and return its reduced body as a self-contained
    * NbE binding, or [[None]] if it produced no [[CompilerMonomorphicValue]] / has no reduced body.
    *
    * This is how an ability-dispatched guard's bodied sub-values are reduced *per instantiation* (ability-guards Stage
    * 4): a guard `where E1 != E2` reaches the `Eq` method `equals` only inside `!=`'s body, so `equals` never resolves
    * off the marker's own signature. Reducing `!=` here at its inferred `[Type]` yields a body with `equals` already
    * resolved to `Eq[Type]::equals` (whose body-less native leaf then reduces the concrete comparison), which the
    * signature read-back inlines to fully reduce the guard. Always the compiler track: a guard is a compile-time computation.
    */
  /** `recursive` (default `false`) additionally reduces each dependency at its own instantiation (see
    * [[collectBindings]]) so that a **stacked** carrier resolves through every layer — used only by the inline-guard
    * reader (signature split, Step 7), where an impl body's dependency (`AbortCarrier::abort` over the base `Either`)
    * would otherwise close over a still-abstract raw binding. The default keeps the one-hop raw-dependency closure the
    * precompute-and-merge and marker-guard readers rely on (reducing every dependency there would trip spurious
    * per-instantiation ability resolutions — e.g. a `Compare` dependency at a defaulted `Type` argument).
    */
  def reduceInstance(
      vfqn: ValueFQN,
      typeArguments: Seq[GroundValue],
      recursive: Boolean = false
  ): CompilerIO[Option[SemValue]] =
    getFactIfProduced(CompilerMonomorphicValue.Key(vfqn, typeArguments)).flatMap {
      case Some(cmv) => cmv.reduced.traverse(r => buildBinding(vfqn, r.value, Platform.Compiler, recursive))
      case None      => None.pure[CompilerIO]
    }

  /** Build the NbE binding for `vfqn` from its reduced compile-time body, resolving each dependency's binding in the
    * given `platform` pool. The dependencies are resolved once here; the resulting `VTopDef` thunk is self-contained, so
    * evaluating it later needs no further fact lookup.
    */
  def buildBinding(
      vfqn: ValueFQN,
      reduced: MonomorphicExpression.Expression,
      platform: Platform,
      recursive: Boolean = false
  ): CompilerIO[SemValue] =
    for {
      deps <- collectBindings(reduced, vfqn, platform, recursive)
    } yield VTopDef(
      vfqn,
      Some(Lazy {
        val evaluator = new MonomorphicEvaluator(ref => deps.get(ref))
        evaluator.eval(Env.empty, MonomorphicExpression(GroundValue.Type, reduced))
      }),
      Spine.SNil
    )

  /** Collect the binding of every value reference in the reduced body, skipping the value's own FQN and any FQN already
    * an ancestor on the active fact-request chain (the same mutual-recursion guard [[BindingClosure]] uses). By default
    * each dependency is its raw [[NativeBinding]] body (a one-hop closure).
    *
    * When `recursive` (the inline-guard reader), a dependency is instead taken **reduced at its own instantiation**
    * ([[reduceInstance]], recursively, against the reference's ground type arguments), so a **stacked** carrier resolves
    * through every layer: when this reduced body fell back to *structural* (an unreduced value parameter, e.g. `if`'s
    * `condition`, left it un-normal-formed), its impl-dispatch dependency (`AbortCarrier::abort` = `AbortCarrier(pure(None))`)
    * still gets its nested base ability (`pure`) resolved rather than closing over the raw, still-abstract binding.
    * `MonomorphicEvaluator` drops a reference's type arguments, so the already-instantiated reduced dependency is used
    * directly with no re-application. A dependency with no reduced form (a native leaf, a `data` constructor) falls back
    * to its raw [[NativeBinding]] body.
    */
  private def collectBindings(
      reduced: MonomorphicExpression.Expression,
      selfFqn: ValueFQN,
      platform: Platform,
      recursive: Boolean
  ): CompilerIO[Map[ValueFQN, SemValue]] =
    valueReferences(reduced).filterNot(_._1 == selfFqn).toList.foldLeftM(Map.empty[ValueFQN, SemValue]) {
      case (acc, (fqn, typeArgs)) =>
        activeFactKeys.flatMap { ancestors =>
          if (ancestors.contains(NativeBinding.Key(fqn, platform))) acc.pure[CompilerIO]
          else {
            def raw: CompilerIO[Map[ValueFQN, SemValue]] =
              getFactIfProduced(NativeBinding.Key(fqn, platform)).map {
                case Some(binding) => acc + (fqn -> binding.semValue)
                case None          => acc
              }
            if (recursive)
              reduceInstance(fqn, typeArgs, recursive = true).flatMap {
                case Some(reduced) => (acc + (fqn -> reduced)).pure[CompilerIO]
                case None          => raw
              }
            else raw
          }
        }
    }

  private def valueReferences(
      expr: MonomorphicExpression.Expression
  ): Set[(ValueFQN, Seq[GroundValue])] = expr match {
    case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) => Set((vfqn.value, typeArgs))
    case MonomorphicExpression.FunctionApplication(target, argument)     =>
      valueReferences(target.value.expression) ++ valueReferences(argument.value.expression)
    case MonomorphicExpression.FunctionLiteral(_, _, body)              => valueReferences(body.value.expression)
    case _                                                              => Set.empty
  }
}
