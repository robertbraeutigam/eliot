package com.vanillasource.eliot.eliotc.typesystem

import cats.Show
import cats.data.StateT
import cats.effect.IO
import cats.implicits.*
import cats.kernel.Monoid
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.resolve.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.{GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*

case class TypeUnification private (
    genericParameters: Map[String, GenericParameter],
    assignments: Seq[(TypeReference, TypeReference)]
) {
  def solve()(using CompilationProcess): CompilationIO[Unit] =
    assignments
      .traverse(pair => StateT.modifyF(state => solve(state, pair._1, pair._2)))
      .runS(TypeUnificationState())
      .void

  private def solve(state: TypeUnificationState, target: TypeReference, source: TypeReference)(using
      CompilationProcess
  ): CompilationIO[TypeUnificationState] = {
    val targetCurrent = state.getCurrentType(target)
    val sourceCurrent = state.getCurrentType(source)

    unify(targetCurrent, sourceCurrent).map(unifiedType => state.unifyTo(target, source, unifiedType))
  }

  private def unify(current: TypeReference, incoming: TypeReference)(using
      CompilationProcess
  ): CompilationIO[TypeReference] =
    current match
      case DirectTypeReference(currentType)                                    =>
        incoming match
          case DirectTypeReference(incomingType) if currentType.value === incomingType.value =>
            current.pure[CompilationIO] // Same type, so return current one
          case DirectTypeReference(incomingType)                                             =>
            compilerError(
              incomingType.as(
                s"Expression with type ${incomingType.value.show} can not be assigned to type ${currentType.value.show}."
              )
            ).as(current)
          case GenericTypeReference(_)                                                       =>
            current.pure[CompilationIO] // Incoming more generic, so return current one
      case GenericTypeReference(currentType) if isUniversal(currentType.value) =>
        incoming match
          case DirectTypeReference(incomingType)                                     =>
            compilerError(
              incomingType.as(
                s"Expression with type ${incomingType.value.show} can not be assigned to universal generic type ${currentType.value.show}."
              )
            ).as(current)
          case GenericTypeReference(incomingType) if isUniversal(incomingType.value) =>
            compilerError(
              incomingType.as(
                s"Expression with universal generic type ${incomingType.value.show} can not be assigned to universal generic type ${currentType.value.show}."
              )
            ).whenA(incomingType.value =!= currentType.value).as(current)
          case GenericTypeReference(_)                                               => current.pure[CompilationIO] // Nothing's changed, no constraints
      case GenericTypeReference(_)                                             =>
        incoming match
          case DirectTypeReference(_)  =>
            incoming.sourcedAt(current).pure[CompilationIO] // Switch to more concrete type
          case GenericTypeReference(_) => current.pure[CompilationIO] // Nothing's changed, no constraints

  private def isUniversal(genericTypeName: String) =
    genericParameters.get(genericTypeName).exists(_.isInstanceOf[UniversalGenericParameter])
}

object TypeUnification {
  def genericParameters(genericParameters: Seq[GenericParameter]): TypeUnification =
    TypeUnification(genericParameters.map(e => e.name.value -> e).toMap, Seq.empty)

  def assignment(target: TypeReference, source: TypeReference): TypeUnification =
    TypeUnification(Map.empty, Seq((target, source)))

  given Monoid[TypeUnification] = new Monoid[TypeUnification] {
    override def empty: TypeUnification = TypeUnification(Map.empty, Seq.empty)

    override def combine(left: TypeUnification, right: TypeUnification): TypeUnification =
      TypeUnification(
        left.genericParameters ++ right.genericParameters,
        left.assignments ++ right.assignments
      )
  }

  given Show[TypeUnification] = (unification: TypeUnification) =>
    unification.genericParameters.values.map {
      case GenericParameter.ExistentialGenericParameter(name) => s"∃${name.value}"
      case GenericParameter.UniversalGenericParameter(name)   => s"∀${name.value}"
    }.mkString +
      ": " +
      unification.assignments
        .map((target, source) => s"${target.show} <- ${source.show}")
        .mkString(" ∧ ")
}
