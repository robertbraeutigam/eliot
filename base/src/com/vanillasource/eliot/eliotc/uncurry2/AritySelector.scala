package com.vanillasource.eliot.eliotc.uncurry2

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.used2.UsedNames.UsageStats

object AritySelector {

  /** Select the optimal arity for uncurrying based on usage statistics.
    *
    * Strategy: Choose the arity with the highest usage frequency, but never exceed the function's maximum arity (from
    * its type).
    *
    * @param stats
    *   Usage statistics from UsedNames
    * @param signature
    *   The function's type signature (to determine max arity)
    * @return
    *   The optimal arity to uncurry to
    */
  def selectOptimalArity(stats: Option[UsageStats], signature: Value): Int = {
    val maxArity = computeMaxArity(signature)

    stats match {
      case Some(usageStats) if usageStats.directCallApplications.nonEmpty =>
        val (bestArity, _) = usageStats.directCallApplications
          .filter { case (arity, _) => arity <= maxArity }
          .maxByOption { case (arity, count) => (count, arity) }
          .getOrElse((maxArity, 0))

        bestArity

      case _ =>
        maxArity
    }
  }

  /** Compute the maximum possible arity from a function signature. Counts nested Function types.
    *
    * Example: A -> B -> C -> D has max arity 3
    */
  def computeMaxArity(signature: Value): Int =
    signature match {
      case FunctionType(_, returnType) =>
        1 + computeMaxArity(returnType)
      case _                           =>
        0
    }

  /** Extract parameter type and return type from a function Value.
    */
  object FunctionType {
    def unapply(value: Value): Option[(Value, Value)] =
      value match {
        case Value.Structure(fields, Value.Type) if isFunctionType(fields) =>
          for {
            paramType  <- fields.get("A")
            returnType <- fields.get("B")
          } yield (paramType, returnType)
        case _                                                              =>
          None
      }

    private def isFunctionType(fields: Map[String, Value]): Boolean =
      fields.get("$typeName") match {
        case Some(Value.Direct(vfqn: ValueFQN, _)) => vfqn == Types.functionDataTypeFQN
        case _                                     => false
      }
  }
}
