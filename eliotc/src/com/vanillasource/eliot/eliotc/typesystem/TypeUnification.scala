package com.vanillasource.eliot.eliotc.typesystem

import cats.Show
import cats.kernel.Semigroup
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.resolve.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.{GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.source.CompilationIO.{CompilationIO, compilerError}
import com.vanillasource.eliot.eliotc.typesystem.TypeUnification.Node

case class TypeUnification(genericParameters: Map[String, GenericParameter], nodes: Map[String, Node]) {
  def solve()(using CompilationProcess): CompilationIO[Unit] = nodes.values.toList.traverse_(solve)

  private def solve(node: Node)(using CompilationProcess): CompilationIO[TypeReference] =
    node.equivalence.foldLeftM(node.originType)(unify)

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

  def addAssignment(target: TypeReference, source: TypeReference): TypeUnification =
    TypeUnification(
      genericParameters,
      nodes.updatedWith(target.name) {
        case Some(node) => Some(Node(node.originType, source +: node.equivalence))
        case None       => Some(Node(target, Seq(source)))
      }
    )

  def addGenericParameter(genericParameter: GenericParameter): TypeUnification =
    TypeUnification(
      genericParameters ++ Map((genericParameter.name.value, genericParameter)),
      nodes
    )
}

object TypeUnification {
  def genericParameters(genericParameters: Seq[GenericParameter]): TypeUnification =
    genericParameters.foldLeft(new TypeUnification(Map.empty, Map.empty))(_.addGenericParameter(_))

  def assignment(target: TypeReference, source: TypeReference): TypeUnification =
    new TypeUnification(Map.empty, Map.empty).addAssignment(target, source)

  case class Node(originType: TypeReference, equivalence: Seq[TypeReference])

  given Semigroup[Node] = (left: Node, right: Node) => Node(left.originType, left.equivalence ++ right.equivalence)

  given Semigroup[TypeUnification] = (left: TypeUnification, right: TypeUnification) =>
    TypeUnification(
      left.genericParameters ++ right.genericParameters, // These should be unique, so no merge
      (left.nodes.toSeq ++ right.nodes.toSeq).groupMapReduce(_._1)(_._2)(_ combine _)
    )

  given Show[TypeUnification] = (unification: TypeUnification) =>
    unification.genericParameters.values.map {
      case GenericParameter.ExistentialGenericParameter(name) => s"∃${name.value}"
      case GenericParameter.UniversalGenericParameter(name)   => s"∀${name.value}"
    }.mkString +
      ": " +
      unification.nodes
        .map((name, ns) => s"$name <- ${ns.equivalence.map(_.show).mkString(", ")}")
        .mkString(" ∧ ")
}
