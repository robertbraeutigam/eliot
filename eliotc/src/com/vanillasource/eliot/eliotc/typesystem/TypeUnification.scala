package com.vanillasource.eliot.eliotc.typesystem

import cats.Show
import cats.data.StateT
import cats.effect.IO
import cats.implicits.*
import cats.kernel.Monoid
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.{GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeUnification.Assignment

case class TypeUnification private (
    genericParameters: Map[String, GenericParameter],
    assignments: Seq[Assignment]
) {
  def solve()(using CompilationProcess): CompilationIO[Unit] =
    assignments
      .traverse(solve)
      .runS(TypeUnificationState())
      .void

  private def solve(assignment: Assignment)(using
      CompilationProcess
  ): StateT[CompilationIO, TypeUnificationState, Unit] =
    for {
      targetCurrent <- StateT.get[CompilationIO, TypeUnificationState].map(_.getCurrentType(assignment.target))
      sourceCurrent <- StateT.get[CompilationIO, TypeUnificationState].map(_.getCurrentType(assignment.source.value))
      unifiedType   <- StateT.liftF(unify(assignment.refocus(targetCurrent, sourceCurrent)))
      _             <- StateT.modify[CompilationIO, TypeUnificationState](
                         _.unifyTo(assignment.target, assignment.source.value, unifiedType)
                       )
      _             <-
        (targetCurrent.genericParameters zip sourceCurrent.genericParameters.map(assignment.source.as))
          .map { case (targetGeneric, sourceGeneric) =>
            assignment.refocus(targetGeneric, sourceGeneric).withErrorMessage("Type mismatch.")
          }
          .traverse(solve)
          .whenA(targetCurrent.identifier =!= sourceCurrent.identifier)
    } yield ()

  private def unify(assignment: Assignment)(using CompilationProcess): CompilationIO[TypeReference] = {
    given Show[TypeFQN] = TypeFQN.fullyQualified

    assignment.target match
      case DirectTypeReference(currentType, _)                                                           =>
        assignment.source.value match
          case DirectTypeReference(incomingType, _) if currentType.value === incomingType.value =>
            // Both direct references _must_ have the correct arity per resolver
            assignment.target.pure[CompilationIO] // Same type, so return assignment.target one
          case DirectTypeReference(_, _)                                                        =>
            compilerError(
              assignment.source.as(assignment.errorMessage),
              Seq(
                s"Expected: ${TypeReference.unqualified.show(assignment.target)}",
                s"Found:    ${TypeReference.unqualified.show(assignment.source.value)}"
              )
            ).as(assignment.target)
          case GenericTypeReference(incomingType, genericParameters)                            =>
            // Note: the direct type needs to have the generic parameters defined
            if (genericParameters.isEmpty || genericParameters.length === assignment.target.genericParameters.length) {
              assignment.target.pure[CompilationIO] // Incoming more generic, so return assignment.target one
            } else {
              compilerError(
                assignment.source.as(
                  s"Expression with type ${incomingType.value.show} can not be assigned to type ${currentType.value.show}, because they have different number of generic parameters."
                )
              ).as(assignment.target)
            }
      case GenericTypeReference(currentType, currentGenericParameters) if isUniversal(currentType.value) =>
        assignment.source.value match
          case DirectTypeReference(incomingType, _)                                     =>
            compilerError(
              assignment.source.as(
                s"Expression with type ${incomingType.value.show} can not be assigned to universal generic type ${currentType.value.show}."
              )
            ).as(assignment.target)
          case GenericTypeReference(incomingType, _) if isUniversal(incomingType.value) =>
            compilerError(
              assignment.source.as(
                s"Expression with universal generic type ${incomingType.value.show} can not be assigned to universal generic type ${currentType.value.show}."
              )
            ).whenA(incomingType.value =!= currentType.value).as(assignment.target)
          case GenericTypeReference(_, incomingGenericParameters)                       =>
            if (
              incomingGenericParameters.isEmpty || currentGenericParameters.isEmpty || incomingGenericParameters.length === currentGenericParameters.length
            ) {
              // Add generic parameter restrictions (remember the more restrictive type reference)
              if (incomingGenericParameters.isEmpty) {
                assignment.target.pure[CompilationIO]
              } else {
                assignment.source.value.sourcedAt(assignment.target).pure[CompilationIO]
              }
            } else {
              compilerError(
                assignment.source.as("Type mismatch, different number of generic parameters."),
                Seq(
                  s"Expected: ${TypeReference.unqualified.show(assignment.target)}",
                  s"Found:    ${TypeReference.unqualified.show(assignment.source.value)}"
                )
              ).as(assignment.target)
            }
      case GenericTypeReference(_, currentGenericParameters)                                             =>
        assignment.source.value match
          case DirectTypeReference(_, incomingGenericParameters)             =>
            if (
              currentGenericParameters.isEmpty || currentGenericParameters.length === incomingGenericParameters.length
            ) {
              assignment.source.value.sourcedAt(assignment.target).pure[CompilationIO] // Switch to more concrete type
            } else {
              compilerError(
                assignment.source.as("Type mismatch, different number of generic parameters."),
                Seq(
                  s"Expected: ${TypeReference.unqualified.show(assignment.target)}",
                  s"Found:    ${TypeReference.unqualified.show(assignment.source.value)}"
                )
              ).as(assignment.target)
            }
          case GenericTypeReference(incomingType, incomingGenericParameters) =>
            if (
              incomingGenericParameters.isEmpty || currentGenericParameters.isEmpty || incomingGenericParameters.length === currentGenericParameters.length
            ) {
              // If assignment.source is more restrictive (has parameters or is universal), pick that
              if (incomingGenericParameters.nonEmpty || isUniversal(incomingType.value)) {
                assignment.source.value.sourcedAt(assignment.target).pure[CompilationIO]
              } else {
                assignment.target.pure[CompilationIO]
              }
            } else {
              compilerError(
                assignment.source.as("Type mismatch, different number of generic parameters."),
                Seq(
                  s"Expected: ${TypeReference.unqualified.show(assignment.target)}",
                  s"Found:    ${TypeReference.unqualified.show(assignment.source.value)}"
                )
              ).as(assignment.target)
            }
  }

  private def isUniversal(genericTypeName: String) =
    genericParameters.get(genericTypeName).exists(_.isInstanceOf[UniversalGenericParameter])
}

object TypeUnification {
  case class Assignment(
      target: TypeReference,
      source: Sourced[TypeReference],
      errorMessage: String
  ) {
    def refocus(newTarget: TypeReference, newSource: TypeReference): Assignment =
      copy(target = newTarget, source = source.as(newSource))

    def refocus(newTarget: TypeReference, newSource: Sourced[TypeReference]): Assignment =
      copy(target = newTarget, source = newSource)

    def withErrorMessage(newErrorMessage: String): Assignment =
      copy(errorMessage = newErrorMessage)
  }

  def genericParameters(genericParameters: Seq[GenericParameter]): TypeUnification =
    TypeUnification(genericParameters.map(e => e.name.value -> e).toMap, Seq.empty)

  def genericParameter(genericParameter: GenericParameter): TypeUnification =
    genericParameters(Seq(genericParameter))

  def assignment(
      target: TypeReference,
      source: Sourced[TypeReference],
      errorMessage: String
  ): TypeUnification =
    TypeUnification(Map.empty, Seq(Assignment(target, source, errorMessage)))

  given Monoid[TypeUnification] = new Monoid[TypeUnification] {
    override def empty: TypeUnification = TypeUnification(Map.empty, Seq.empty)

    override def combine(left: TypeUnification, right: TypeUnification): TypeUnification =
      TypeUnification(
        left.genericParameters ++ right.genericParameters,
        left.assignments ++ right.assignments
      )
  }

  given Show[TypeUnification] = (unification: TypeUnification) =>
    unification.genericParameters.values
      .collect { case GenericParameter.UniversalGenericParameter(name, _) =>
        s"∀${name.value}"
      }
      .mkString(", ") +
      ": " +
      unification.assignments
        .map(assignment =>
          s"${TypeReference.unqualified.show(assignment.target)} <- ${TypeReference.unqualified.show(assignment.source.value)}"
        )
        .mkString(" ∧ ")
}
