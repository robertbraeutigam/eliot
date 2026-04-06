package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints.Constraint
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*

/** Decomposes ORE constraints by structurally peeling off matching constructors. Produces finer-grained
  * sub-constraints. Runs entirely on ORE without evaluation, so structural information like value reference names and
  * type arguments is preserved before they would be lost in conversion to ExpressionValue.
  */
object StructuralDecomposition {

  def decompose(constraints: Constraints): Seq[Constraint] =
    constraints.constraints.flatMap(decomposeOne)

  private def decomposeOne(constraint: Constraint): Seq[Constraint] =
    (constraint.left, constraint.right.value) match {
      case (FunctionApplication(lt, la), FunctionApplication(rt, ra)) =>
        decomposeOne(Constraint(lt.value, rt, constraint.errorMessage)) ++
          decomposeOne(Constraint(la.value, ra, constraint.errorMessage))
      case (ValueReference(ln, largs), ValueReference(rn, rargs))
          if ln.value == rn.value && largs.length == rargs.length =>
        largs.zip(rargs).flatMap { case (la, ra) =>
          decomposeOne(Constraint(la.value, ra, constraint.errorMessage))
        }
      case _                                                          =>
        Seq(constraint)
    }
}
