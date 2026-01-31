package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.monomorphize.fact.ConcreteType
import com.vanillasource.eliot.eliotc.monomorphize.fact.ConcreteType.*

/** Structural equality checking for ConcreteTypes. Used to verify that expected and actual types match during
  * monomorphic type checking.
  */
object ConcreteTypeEquality {

  /** Check if two concrete types are structurally equal.
    */
  def areEqual(t1: ConcreteType, t2: ConcreteType): Boolean =
    (t1, t2) match {
      case (TypeRef(v1), TypeRef(v2)) =>
        v1 == v2

      case (FunctionType(p1, r1), FunctionType(p2, r2)) =>
        areEqual(p1, p2) && areEqual(r1, r2)

      case (TypeApplication(t1, a1), TypeApplication(t2, a2)) =>
        areEqual(t1, t2) && areEqual(a1, a2)

      case (IntLiteral(n1), IntLiteral(n2)) =>
        n1 == n2

      case (StringLiteral(s1), StringLiteral(s2)) =>
        s1 == s2

      case _ => false
    }

  /** Check if actual type is compatible with expected type. For now this is strict equality, but could be extended for
    * subtyping in the future.
    */
  def isCompatible(expected: ConcreteType, actual: ConcreteType): Boolean =
    areEqual(expected, actual)
}
