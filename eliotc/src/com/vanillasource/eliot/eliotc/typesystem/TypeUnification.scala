package com.vanillasource.eliot.eliotc.typesystem

import cats.Show
import cats.data.StateT
import cats.effect.IO
import cats.implicits.*
import cats.kernel.Monoid
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.{GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced

case class TypeUnification private (
    genericParameters: Map[String, GenericParameter],
    assignments: Seq[(TypeReference, Sourced[TypeReference])]
) {
  def solve()(using CompilationProcess): CompilationIO[Unit] =
    assignments
      .traverse(solve.tupled)
      .runS(TypeUnificationState())
      .void

  private def solve(target: TypeReference, source: Sourced[TypeReference])(using
      CompilationProcess
  ): StateT[CompilationIO, TypeUnificationState, Unit] = for {
    targetCurrent <- StateT.get[CompilationIO, TypeUnificationState].map(_.getCurrentType(target))
    sourceCurrent <- StateT.get[CompilationIO, TypeUnificationState].map(_.getCurrentType(source.value))
    unifiedType   <- StateT.liftF(unify(targetCurrent, source.as(sourceCurrent)))
    _             <- StateT.modify[CompilationIO, TypeUnificationState](_.unifyTo(target, source.value, unifiedType))
    _             <- (targetCurrent.genericParameters zip sourceCurrent.genericParameters.map(source.as)).traverse(solve.tupled)
  } yield ()

  private def unify(current: TypeReference, incoming: Sourced[TypeReference])(using
      CompilationProcess
  ): CompilationIO[TypeReference] =
    current match
      case DirectTypeReference(currentType, _)                                                           =>
        incoming.value match
          case DirectTypeReference(incomingType, _) if currentType.value === incomingType.value =>
            // Both direct references _must_ have the correct arity per resolver
            current.pure[CompilationIO] // Same type, so return current one
          case DirectTypeReference(incomingType, _)                                             =>
            compilerError(
              incoming.as(
                s"Expression with type ${incomingType.value.show} can not be assigned to type ${currentType.value.show}."
              )
            ).as(current)
          case GenericTypeReference(incomingType, genericParameters)                            =>
            // Note: the direct type needs to have the generic parameters defined
            if (genericParameters.isEmpty || genericParameters.length === current.genericParameters.length) {
              current.pure[CompilationIO] // Incoming more generic, so return current one
            } else {
              compilerError(
                incoming.as(
                  s"Expression with type ${incomingType.value.show} can not be assigned to type ${currentType.value.show}, because they have different number of generic parameters."
                )
              ).as(current)
            }
      case GenericTypeReference(currentType, currentGenericParameters) if isUniversal(currentType.value) =>
        incoming.value match
          case DirectTypeReference(incomingType, _)                                     =>
            compilerError(
              incoming.as(
                s"Expression with type ${incomingType.value.show} can not be assigned to universal generic type ${currentType.value.show}."
              )
            ).as(current)
          case GenericTypeReference(incomingType, _) if isUniversal(incomingType.value) =>
            compilerError(
              incoming.as(
                s"Expression with universal generic type ${incomingType.value.show} can not be assigned to universal generic type ${currentType.value.show}."
              )
            ).whenA(incomingType.value =!= currentType.value).as(current)
          case GenericTypeReference(incomingType, incomingGenericParameters)            =>
            if (
              incomingGenericParameters.isEmpty || currentGenericParameters.isEmpty || incomingGenericParameters.length === currentGenericParameters.length
            ) {
              // Add generic parameter restrictions (remember the more restrictive type reference)
              if (incomingGenericParameters.isEmpty) {
                current.pure[CompilationIO]
              } else {
                incoming.value.sourcedAt(current).pure[CompilationIO]
              }
            } else {
              compilerError(
                incoming.as(
                  s"Expression with type ${incomingType.value.show} can not be assigned to type ${currentType.value.show}, because they have different number of generic parameters."
                )
              ).as(current)
            }
      case GenericTypeReference(currentType, currentGenericParameters)                                   =>
        incoming.value match
          case DirectTypeReference(incomingType, incomingGenericParameters)  =>
            if (
              currentGenericParameters.isEmpty || currentGenericParameters.length === incomingGenericParameters.length
            ) {
              incoming.value.sourcedAt(current).pure[CompilationIO] // Switch to more concrete type
            } else {
              compilerError(
                incoming.as(
                  s"Expression with type ${incomingType.value.show} can not be assigned to type ${currentType.value.show}, because they have different number of generic parameters."
                )
              ).as(current)
            }
          case GenericTypeReference(incomingType, incomingGenericParameters) =>
            if (
              incomingGenericParameters.isEmpty || currentGenericParameters.isEmpty || incomingGenericParameters.length === currentGenericParameters.length
            ) {
              // Add generic parameter restrictions (remember the more restrictive type reference)
              if (incomingGenericParameters.isEmpty) {
                current.pure[CompilationIO]
              } else {
                incoming.value.sourcedAt(current).pure[CompilationIO]
              }
            } else {
              compilerError(
                incoming.as(
                  s"Expression with type ${incomingType.value.show} can not be assigned to type ${currentType.value.show}, because they have different number of generic parameters."
                )
              ).as(current)
            }

  private def isUniversal(genericTypeName: String) =
    genericParameters.get(genericTypeName).exists(_.isInstanceOf[UniversalGenericParameter])
}

object TypeUnification {
  def genericParameters(genericParameters: Seq[GenericParameter]): TypeUnification =
    TypeUnification(genericParameters.map(e => e.name.value -> e).toMap, Seq.empty)

  def genericParameter(genericParameter: GenericParameter): TypeUnification =
    genericParameters(Seq(genericParameter))

  def assignment(target: TypeReference, source: Sourced[TypeReference]): TypeUnification =
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
      case GenericParameter.ExistentialGenericParameter(name, _) => s"∃${name.value}"
      case GenericParameter.UniversalGenericParameter(name, _)   => s"∀${name.value}"
    }.mkString +
      ": " +
      unification.assignments
        .map((target, source) => s"${target.show} <- ${source.value.show}")
        .mkString(" ∧ ")
}
