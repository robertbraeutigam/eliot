package com.vanillasource.eliot.eliotc.implementation.util

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.implementation.util.SymbolicType.*

/** A type with universally quantified type parameters separated from the body. This replaces the ad-hoc pattern of
  * encoding universal quantification as leading TypeLambdas in a SymbolicType.
  *
  * @param typeParams
  *   The universally quantified type parameter names (in order)
  * @param body
  *   The type body with type variables referencing the parameters
  */
case class QuantifiedType(typeParams: Seq[(String, SymbolicType)], body: SymbolicType)

object QuantifiedType {

  /** Extract leading TypeLambdas from a SymbolicType into a QuantifiedType. */
  def fromSymbolicType(st: SymbolicType): QuantifiedType = {
    def loop(current: SymbolicType, acc: Seq[(String, SymbolicType)]): QuantifiedType =
      current match {
        case TypeLambda(name, parameterType, inner) => loop(inner.value, acc :+ (name, parameterType))
        case body                                   => QuantifiedType(acc, body)
      }
    loop(st, Seq.empty)
  }

  /** Convert back to a SymbolicType by wrapping the body in leading TypeLambdas. */
  def toSymbolicType(qt: QuantifiedType): SymbolicType =
    qt.typeParams.foldRight(qt.body) { (param, acc) =>
      TypeLambda(param._1, param._2, SymbolicType.unsourced(acc))
    }

  given Show[QuantifiedType] with {
    def show(qt: QuantifiedType): String =
      if (qt.typeParams.isEmpty) qt.body.show
      else s"[${qt.typeParams.mkString(", ")}] ${qt.body.show}"
  }
}
