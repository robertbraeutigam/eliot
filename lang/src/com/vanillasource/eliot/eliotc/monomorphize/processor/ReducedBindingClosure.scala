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
    * resolved to `Eq[Type]::equals` (and its `typeEquals` leaf folded in), which the signature read-back inlines to
    * fully reduce the guard. Always the compiler track: a guard is a compile-time computation.
    */
  def reduceInstance(vfqn: ValueFQN, typeArguments: Seq[GroundValue]): CompilerIO[Option[SemValue]] =
    getFactIfProduced(CompilerMonomorphicValue.Key(vfqn, typeArguments)).flatMap {
      case Some(cmv) => cmv.reduced.traverse(r => buildBinding(vfqn, r.value, Platform.Compiler))
      case None      => None.pure[CompilerIO]
    }

  /** Build the NbE binding for `vfqn` from its reduced compile-time body, resolving each dependency's binding in the
    * given `platform` pool. The dependencies are resolved once here; the resulting `VTopDef` thunk is self-contained, so
    * evaluating it later needs no further fact lookup.
    */
  def buildBinding(
      vfqn: ValueFQN,
      reduced: MonomorphicExpression.Expression,
      platform: Platform
  ): CompilerIO[SemValue] =
    for {
      deps <- collectBindings(reduced, vfqn, platform)
    } yield VTopDef(
      vfqn,
      Some(Lazy {
        val evaluator = new MonomorphicEvaluator(ref => deps.get(ref))
        evaluator.eval(Env.empty, MonomorphicExpression(GroundValue.Type, reduced))
      }),
      Spine.SNil
    )

  /** Collect the [[NativeBinding]]s of every value reference in the reduced body, skipping the value's own FQN and any
    * FQN already an ancestor on the active fact-request chain (the same mutual-recursion guard [[BindingClosure]] uses).
    */
  private def collectBindings(
      reduced: MonomorphicExpression.Expression,
      selfFqn: ValueFQN,
      platform: Platform
  ): CompilerIO[Map[ValueFQN, SemValue]] =
    (valueReferences(reduced) - selfFqn).toList.foldLeftM(Map.empty[ValueFQN, SemValue]) { (acc, fqn) =>
      activeFactKeys.flatMap { ancestors =>
        if (ancestors.contains(NativeBinding.Key(fqn, platform))) acc.pure[CompilerIO]
        else
          getFactIfProduced(NativeBinding.Key(fqn, platform)).map {
            case Some(binding) => acc + (fqn -> binding.semValue)
            case None          => acc
          }
      }
    }

  private def valueReferences(expr: MonomorphicExpression.Expression): Set[ValueFQN] = expr match {
    case MonomorphicExpression.MonomorphicValueReference(vfqn, _)    => Set(vfqn.value)
    case MonomorphicExpression.FunctionApplication(target, argument) =>
      valueReferences(target.value.expression) ++ valueReferences(argument.value.expression)
    case MonomorphicExpression.FunctionLiteral(_, _, body)          => valueReferences(body.value.expression)
    case _                                                          => Set.empty
  }
}
