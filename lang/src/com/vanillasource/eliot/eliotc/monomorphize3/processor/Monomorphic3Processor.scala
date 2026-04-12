package com.vanillasource.eliot.eliotc.monomorphize3.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize3.check.TypeStackLoop
import com.vanillasource.eliot.eliotc.monomorphize3.domain.{Env, SemValue}
import com.vanillasource.eliot.eliotc.monomorphize3.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize3.fact.{Monomorphic3Value, NativeBinding}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

/** Entry point for NbE-based type checking (monomorphize3). Delegates to TypeStackLoop for the actual type checking
  * work.
  */
class Monomorphic3Processor
    extends TransformationProcessor[OperatorResolvedValue.Key, Monomorphic3Value.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  private def fetchBinding(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    getFact(NativeBinding.Key(vfqn)).map(_.map(_.semValue))

  override protected def generateFromKeyAndFact(
      key: Monomorphic3Value.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[Monomorphic3Value] =
    TypeStackLoop.process(
      key,
      resolvedValue,
      fetchBinding = fetchBinding,
      fetchValueType = vfqn => fetchEvaluatedSignature(vfqn)
    )

  /** Fetch a value's type stack signature, evaluate it to a SemValue. Uses NativeBinding lookups for proper resolution.
    */
  private def fetchEvaluatedSignature(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    getFact(OperatorResolvedValue.Key(vfqn)).flatMap {
      case Some(orv) =>
        val levels = orv.typeStack.value.levels.toSeq
        for {
          bindings <- collectBindings(levels)
        } yield {
          val evaluator = new Evaluator(v => bindings.get(v), Map.empty)
          val reversed  = levels.reverse
          val result    = reversed.foldLeft(SemValue.VType.asInstanceOf[SemValue]) { (_, level) =>
            evaluator.eval(Env.empty, level)
          }
          Some(result)
        }
      case None      => None.pure[CompilerIO]
    }

  /** Recursively collect all NativeBindings referenced in ORE expressions. */
  private def collectBindings(
      levels: Seq[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression]
  ): CompilerIO[Map[ValueFQN, SemValue]] = {
    import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression as ORE
    def collect(ore: ORE, acc: Map[ValueFQN, SemValue]): CompilerIO[Map[ValueFQN, SemValue]] = ore match {
      case ORE.ValueReference(vfqn, typeArgs) =>
        if (acc.contains(vfqn.value)) typeArgs.foldLeft(acc.pure[CompilerIO])((a, ta) => a.flatMap(collect(ta.value, _)))
        else
          fetchBinding(vfqn.value).flatMap {
            case Some(sem) =>
              val newAcc = acc + (vfqn.value -> sem)
              typeArgs.foldLeft(newAcc.pure[CompilerIO])((a, ta) => a.flatMap(collect(ta.value, _)))
            case None      => acc.pure[CompilerIO]
          }
      case ORE.FunctionApplication(target, arg) =>
        collect(target.value, acc).flatMap(collect(arg.value, _))
      case ORE.FunctionLiteral(_, paramType, body) =>
        val withParamType = paramType.foldLeft(acc.pure[CompilerIO]) { (a, pt) =>
          pt.value.levels.toSeq.foldLeft(a)((a2, level) => a2.flatMap(collect(level, _)))
        }
        withParamType.flatMap(collect(body.value, _))
      case _ => acc.pure[CompilerIO]
    }
    levels.foldLeft(Map.empty[ValueFQN, SemValue].pure[CompilerIO])((a, level) => a.flatMap(collect(level, _)))
  }
}
