package com.vanillasource.eliot.eliotc.typesystem.types

import cats.Show
import cats.data.StateT
import cats.effect.IO
import cats.implicits.*
import cats.syntax.all.*
import cats.kernel.Monoid
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.{GenericParameter, TypeReference}
import TypeUnification.Assignment
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.CompilationProcess
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

case class TypeUnification private (
    genericParameters: Map[String, GenericParameter],
    assignments: Seq[Assignment]
) {
  def solve(): CompilerIO[TypeUnificationState] =
    assignments
      .traverse(solve)
      .runS(TypeUnificationState())

  private def solve(assignment: Assignment): StateT[CompilerIO, TypeUnificationState, Unit] =
    for {
      targetCurrent <- StateT.get[CompilerIO, TypeUnificationState].map(_.getCurrentType(assignment.target))
      sourceCurrent <- StateT.get[CompilerIO, TypeUnificationState].map(_.getCurrentType(assignment.source.value))
      unifiedType   <- StateT.liftF(unify(assignment.refocus(targetCurrent, sourceCurrent)))
      _             <- StateT.modify[CompilerIO, TypeUnificationState](
                         _.unifyTo(assignment.target, assignment.source.value, unifiedType)
                       )
      _             <-
        (targetCurrent.genericParameters zip sourceCurrent.genericParameters.map(assignment.source.as))
          .map { case (targetGeneric, sourceGeneric) =>
            assignment
              .makeParents()
              .refocus(targetGeneric, sourceGeneric)
              .withErrorMessage("Type mismatch.")
          }
          .traverse(solve)
          .whenA(targetCurrent.identifier =!= sourceCurrent.identifier)
    } yield ()

  private def unify(assignment: Assignment): CompilerIO[TypeReference] = {
    given Show[TypeFQN] = TypeFQN.fullyQualified

    assignment.target match
      case DirectTypeReference(currentType, _)                                                           =>
        assignment.source.value match
          case DirectTypeReference(incomingType, _) if currentType.value === incomingType.value =>
            // Both direct references _must_ have the correct arity per resolver
            assignment.target.pure[CompilerIO] // Same type, so return assignment.target one
          case DirectTypeReference(_, _)                                                        =>
            assignment.issueError().as(assignment.target)
          case GenericTypeReference(incomingType, genericParameters)                            =>
            // Note: the direct type needs to have the generic parameters defined
            if (genericParameters.isEmpty || genericParameters.length === assignment.target.genericParameters.length) {
              assignment.target.pure[CompilerIO] // Incoming more generic, so return assignment.target one
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
                assignment.target.pure[CompilerIO]
              } else {
                assignment.source.value.sourcedAt(assignment.target).pure[CompilerIO]
              }
            } else {
              assignment.issueError().as(assignment.target)
            }
      case GenericTypeReference(_, currentGenericParameters)                                             =>
        assignment.source.value match
          case DirectTypeReference(_, incomingGenericParameters)             =>
            if (
              currentGenericParameters.isEmpty || currentGenericParameters.length === incomingGenericParameters.length
            ) {
              assignment.source.value.sourcedAt(assignment.target).pure[CompilerIO] // Switch to more concrete type
            } else {
              assignment.issueError().as(assignment.target)
            }
          case GenericTypeReference(incomingType, incomingGenericParameters) =>
            if (
              incomingGenericParameters.isEmpty || currentGenericParameters.isEmpty || incomingGenericParameters.length === currentGenericParameters.length
            ) {
              // If assignment.source is more restrictive (has parameters or is universal), pick that
              if (incomingGenericParameters.nonEmpty || isUniversal(incomingType.value)) {
                assignment.source.value.sourcedAt(assignment.target).pure[CompilerIO]
              } else {
                assignment.target.pure[CompilerIO]
              }
            } else {
              assignment.issueError().as(assignment.target)
            }
  }

  private def isUniversal(genericTypeName: String) =
    genericParameters.get(genericTypeName).exists(_.isInstanceOf[UniversalGenericParameter])

  def printTypes(solution: TypeUnificationState)(using CompilationProcess): CompilerIO[Unit] = {
    assignments.traverse_ { assignment =>
      compilerError(
        assignment.source.as("Type debug"),
        Seq(
          s"Original type: ${TypeReference.unqualified.show(assignment.source.value)}",
          s"  Solved type: ${TypeReference.unqualified.show(solution.getCurrentType(assignment.source.value))}"
        )
      )
    }
  }

  def getSourceType(source: Sourced[?]): Option[TypeReference] =
    assignments.find(_.source.range === source.range).map(_.source.value)
}

object TypeUnification {
  case class Assignment(
      target: TypeReference,
      source: Sourced[TypeReference],
      errorMessage: String,
      targetParent: Option[TypeReference],
      sourceParent: Option[TypeReference]
  ) {
    def refocus(newTarget: TypeReference, newSource: TypeReference): Assignment =
      copy(target = newTarget, source = source.as(newSource))

    def refocus(newTarget: TypeReference, newSource: Sourced[TypeReference]): Assignment =
      copy(target = newTarget, source = newSource)

    def withErrorMessage(newErrorMessage: String): Assignment =
      copy(errorMessage = newErrorMessage)

    def makeParents(): Assignment =
      copy(targetParent = targetParent.orElse(Some(target)), sourceParent = sourceParent.orElse(Some(source.value)))

    def issueError(): CompilerIO[Unit] =
      compilerError(
        source.as(errorMessage),
        typeDescriptions() ++ parentDescriptions()
      )

    private def typeDescriptions(): Seq[String] =
      Seq(
        s"Expected: ${TypeReference.unqualified.show(target)}",
        s"Found:    ${TypeReference.unqualified.show(source.value)}"
      )

    private def parentDescriptions(): Seq[String] =
      (for {
        tp <- targetParent
        sp <- sourceParent
      } yield Seq(
        "from encompassing types:",
        s"Expected: ${TypeReference.unqualified.show(tp)}",
        s"Found:    ${TypeReference.unqualified.show(sp)}"
      )).getOrElse(Seq.empty)
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
    TypeUnification(Map.empty, Seq(Assignment(target, source, errorMessage, None, None)))

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
