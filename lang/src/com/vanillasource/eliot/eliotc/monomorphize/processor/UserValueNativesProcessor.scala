package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.NativeBinding
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

/** Emits NativeBinding facts for user-defined values. Each value is represented as a VTopDef with a lazy thunk that
  * evaluates the value's body on demand.
  *
  * Pre-fetches NativeBindings for all ValueReferences in the body so the thunk evaluator can resolve them during
  * type-level computation (e.g., `def one: Person = Person(1)` needs Person's native binding).
  *
  * Uses a concurrent generation guard to prevent fact system deadlocks from mutual recursion: when generating
  * NativeBinding(f), if f's body references g and g's body references f, the second collectBindings skips f (already
  * generating) instead of deadlocking.
  */
class UserValueNativesProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, NativeBinding.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  /** Thread-safe set of FQNs currently being generated. Prevents mutual recursion deadlocks in collectBindings. */
  private val generating: java.util.Set[ValueFQN] =
    java.util.Collections.newSetFromMap(new java.util.concurrent.ConcurrentHashMap())

  override protected def generateFromKeyAndFact(key: NativeBinding.Key, fact: InputFact): CompilerIO[OutputFact] = {
    generating.add(key.vfqn)
    for {
      bodyBindings <- fact.runtime match {
                        case Some(body) => collectBindings(body.value, Set(key.vfqn))
                        case None       => Map.empty[ValueFQN, SemValue].pure[CompilerIO]
                      }
    } yield {
      generating.remove(key.vfqn)
      val semValue = VTopDef(
        key.vfqn,
        fact.runtime.map { body =>
          Lazy {
            val evaluator = new Evaluator(vfqn => bodyBindings.get(vfqn), Map.empty)
            evaluator.eval(Env.empty, body.value)
          }
        },
        Spine.SNil
      )
      NativeBinding(key.vfqn, semValue)
    }
  }

  /** Recursively collect NativeBindings for all ValueReferences in an ORE expression. Skips FQNs that are currently
    * being generated (in the `generating` set) to prevent mutual recursion deadlocks.
    */
  private def collectBindings(
      ore: OperatorResolvedExpression,
      seen: Set[ValueFQN]
  ): CompilerIO[Map[ValueFQN, SemValue]] = ore match {
    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      if (seen.contains(vfqn.value) || generating.contains(vfqn.value))
        Map.empty[ValueFQN, SemValue].pure[CompilerIO]
      else
        getFact(NativeBinding.Key(vfqn.value)).flatMap {
          case Some(binding) =>
            val base = Map(vfqn.value -> binding.semValue)
            typeArgs.foldLeft(base.pure[CompilerIO]) { (acc, ta) =>
              for {
                m  <- acc
                ta <- collectBindings(ta.value, seen + vfqn.value)
              } yield m ++ ta
            }
          case None          => Map.empty[ValueFQN, SemValue].pure[CompilerIO]
        }
    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        t <- collectBindings(target.value, seen)
        a <- collectBindings(arg.value, seen)
      } yield t ++ a
    case OperatorResolvedExpression.FunctionLiteral(_, paramType, body) =>
      val ptBindings = paramType match {
        case Some(pt) =>
          pt.value.levels.toSeq.foldLeft(Map.empty[ValueFQN, SemValue].pure[CompilerIO]) { (acc, level) =>
            for {
              m <- acc
              b <- collectBindings(level, seen)
            } yield m ++ b
          }
        case None     => Map.empty[ValueFQN, SemValue].pure[CompilerIO]
      }
      for {
        pt <- ptBindings
        b  <- collectBindings(body.value, seen)
      } yield pt ++ b
    case _                                                             => Map.empty[ValueFQN, SemValue].pure[CompilerIO]
  }
}
