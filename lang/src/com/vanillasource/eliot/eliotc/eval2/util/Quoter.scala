package com.vanillasource.eliot.eliotc.eval2.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.fullyQualifiedNameType
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.eval2.util.MetaState.*

/** Converts fully-solved Sem values back to eval package's Value representation.
  *
  * Returns None if any meta is unsolved or any free parameter remains. The output Value is what gets stored in
  * MonomorphicExpression.expressionType.
  */
object Quoter {

  /** Convert a Sem to a Value after forcing all metavariables. Returns None if the Sem contains unresolved neutrals.
    */
  def quote(sem: Sem): EvalIO[Option[Value]] =
    force(sem).flatMap {
      case Sem.Lit(direct)                 => Some(direct: Value).pure[EvalIO]
      case Sem.TypeUniv                    => Some(Value.Type: Value).pure[EvalIO]
      case Sem.Struct(fqn, fields)         =>
        fields.toList
          .traverse { case (name, fieldSem) => quote(fieldSem).map(_.map(name -> _)) }
          .map { optList =>
            optList.sequence.map { pairs =>
              Value.Structure(pairs.toMap, Value.Type): Value
            }
          }
      case Sem.Lam(_, _, _)                => None.pure[EvalIO]
      case Sem.Neut(Head.Ref(vfqn), Seq()) =>
        val struct = Value.Structure(
          Map("$typeName" -> Value.Direct(vfqn, fullyQualifiedNameType)),
          Value.Type
        )
        Some(struct: Value).pure[EvalIO]
      case Sem.Neut(_, _)                  => None.pure[EvalIO]
    }
}
