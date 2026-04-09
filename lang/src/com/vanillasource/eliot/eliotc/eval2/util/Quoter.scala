package com.vanillasource.eliot.eliotc.eval2.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.fullyQualifiedNameType
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.Sem
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN

/** Converts fully-solved Sem values back to Value for output. Returns None if the Sem contains unresolvable neutral
  * terms (free parameters). Top-level references (Head.Ref) with empty spines are quoted to their corresponding
  * Value.Structure representation.
  */
object Quoter {

  /** Convert a Sem to a Value. Returns None if any free parameter remains or a lambda appears in type position. */
  def quote(sem: Sem): Option[Value] = sem match {
    case Sem.Lit(direct) =>
      Some(direct)

    case Sem.TypeUniv =>
      Some(Value.Type)

    case Sem.Struct(typeFqn, fields) =>
      fields.toList
        .traverse { case (k, v) => quote(v).map(k -> _) }
        .map { quotedFields =>
          val allFields = quotedFields.toMap + ("$typeName" -> Value.Direct(typeFqn, fullyQualifiedNameType))
          Value.Structure(allFields, Value.Type)
        }

    case Sem.Lam(_, _, _) =>
      None

    case Sem.Neut(Sem.Head.Ref(vfqn), spine) if spine.isEmpty =>
      Some(
        Value.Structure(
          Map("$typeName" -> Value.Direct(vfqn, fullyQualifiedNameType)),
          Value.Type
        )
      )

    case Sem.Neut(_, _) =>
      None
  }
}
