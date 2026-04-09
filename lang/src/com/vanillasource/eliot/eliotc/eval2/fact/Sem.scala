package com.vanillasource.eliot.eliotc.eval2.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN

/** The semantic domain for NbE-based evaluation. Represents values in normal form,
  * with metavariables as first-class neutral heads.
  */
sealed trait Sem

object Sem {

  /** A literal value: BigInt, String, etc. */
  case class Lit(direct: Value.Direct) extends Sem

  /** The universe of types. */
  case object TypeUniv extends Sem

  /** A fully-evaluated structure (data type instance, function type, etc). */
  case class Struct(typeFqn: ValueFQN, fields: Map[String, Sem]) extends Sem

  /** A closure: a lambda value waiting to be applied. */
  case class Lam(paramName: String, dom: Sem, body: Closure) extends Sem

  /** A neutral term: a head followed by a spine of arguments. Cannot be reduced further. */
  case class Neut(head: Head, spine: Seq[Sem]) extends Sem

  given Show[Sem] = {
    case Lit(direct)          => direct.value.toString
    case TypeUniv             => "Type"
    case Struct(fqn, fields)  =>
      s"${fqn.show}{${fields.map((k, v) => s"$k=${v.show}").mkString(", ")}}"
    case Lam(name, dom, _)    => s"(λ$name: ${dom.show} → …)"
    case Neut(head, spine)    =>
      val headStr = head.show
      if (spine.isEmpty) headStr
      else s"$headStr(${spine.map(_.show).mkString(", ")})"
  }
}
