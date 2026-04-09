package com.vanillasource.eliot.eliotc.eval2.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.eval2.util.MetaState.EvalIO

/** Converts a fully-solved Sem to a Value. Returns None if any meta is unsolved
  * or any free parameter remains.
  */
object Quoter {

  def quote(sem: Sem): EvalIO[Option[Value]] = sem match {
    case Sem.Lit(direct)         => (Some(direct): Option[Value]).pure[EvalIO]
    case Sem.TypeUniv            => (Some(Value.Type): Option[Value]).pure[EvalIO]
    case Sem.Struct(fqn, fields) =>
      fields.toSeq
        .traverse { case (k, v) => quote(v).map(_.map(k -> _)) }
        .map { quotedOpts =>
          quotedOpts.sequence.map { quotedFields =>
            val fieldsMap = quotedFields.toMap + ("$typeName" -> Value.Direct(fqn, Types.fullyQualifiedNameType))
            Value.Structure(fieldsMap, Value.Type)
          }
        }
    case Sem.Lam(_, _, _)        => (None: Option[Value]).pure[EvalIO]
    case Sem.Neut(_, _)          => (None: Option[Value]).pure[EvalIO]
  }
}
