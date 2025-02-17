package com.vanillasource.eliot.eliotc.typesystem

import cats.Show
import cats.collections.DisjointSets
import cats.effect.IO
import cats.kernel.{Monoid, Semigroup}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.resolve.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.{GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*

import scala.collection.immutable.{AbstractSeq, LinearSeq}

case class TypeUnification private (
    genericParameters: Map[String, GenericParameter],
    groups: DisjointSets[TypeReference]
) {
  def solve()(using CompilationProcess): CompilationIO[Unit] =
    groups.toSets._2.toScalaMap.values.map(_.toList).toList.traverse_(solve)

  private def solve(types: Seq[TypeReference])(using CompilationProcess): CompilationIO[TypeReference] = types match
    case head :: tail => tail.foldLeftM(head)(unify)
    case _            => IO.raiseError(IllegalStateException("Empty type group.")).liftToCompilationIO

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
    TypeUnification(genericParameters.map(e => e.name.value -> e).toMap, DisjointSets())

  def assignment(target: TypeReference, source: TypeReference): TypeUnification =
    TypeUnification(Map.empty, DisjointSets(target, source).union(target, source)._1)

  given Monoid[TypeUnification] = new Monoid[TypeUnification] {
    override def empty: TypeUnification = TypeUnification(Map.empty, DisjointSets())

    override def combine(left: TypeUnification, right: TypeUnification): TypeUnification =
      TypeUnification(
        left.genericParameters ++ right.genericParameters, // These should be unique, so no merge
        fromSets(
          left.groups.toSets._2.toScalaMap.values.map(_.toList).toList ++
            right.groups.toSets._2.toScalaMap.values.map(_.toList).toList
        )
      )

    private def fromSets(sets: Seq[Seq[TypeReference]]): DisjointSets[TypeReference] =
      sets.foldLeft(DisjointSets[TypeReference]()) { (ds, group) =>
        group match {
          case Nil          => ds
          case head :: tail =>
            group.foldLeft(ds) { (acc, a) =>
              (acc + a).union(head, a)._1
            }
        }
      }
  }

  given Show[TypeUnification] = (unification: TypeUnification) =>
    unification.genericParameters.values.map {
      case GenericParameter.ExistentialGenericParameter(name) => s"∃${name.value}"
      case GenericParameter.UniversalGenericParameter(name)   => s"∀${name.value}"
    }.mkString +
      ": " +
      unification.groups.toSets._2.toScalaMap.values
        .map(_.toList)
        .toList
        .map(_.map(_.show).mkString("unify(", ", ", ")"))
        .mkString(" ∧ ")
}
