package com.vanillasource.eliot.eliotc.typesystem

import cats.kernel.Semigroup
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.resolve.{GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.typesystem.TypeUnificationGraph.Node

case class TypeUnificationGraph(genericParameters: Map[String, GenericParameter], nodes: Map[String, Node]) {
  def addAssignment(target: TypeReference, source: TypeReference): TypeUnificationGraph =
    TypeUnificationGraph(
      genericParameters,
      nodes.updatedWith(target.name) {
        case Some(node) => Some(Node(node.originType, source +: node.equivalence))
        case None       => Some(Node(target, Seq(source)))
      }
    )

  def addGenericParameter(genericParameter: GenericParameter): TypeUnificationGraph =
    TypeUnificationGraph(
      genericParameters ++ Map((genericParameter.name.value, genericParameter)),
      nodes
    )

  def printProblem: String =
    genericParameters.values.map {
      case GenericParameter.ExistentialGenericParameter(name) => s"∃${name.value}"
      case GenericParameter.UniversalGenericParameter(name)   => s"∀${name.value}"
    }.mkString +
      ": " +
      nodes
        .map((name, ns) => s"$name <- ${ns.equivalence.map(_.show).mkString(", ")}")
        .mkString(" ∧ ")
}

object TypeUnificationGraph {
  def genericParameters(genericParameters: Seq[GenericParameter]): TypeUnificationGraph =
    genericParameters.foldLeft(new TypeUnificationGraph(Map.empty, Map.empty))(_.addGenericParameter(_))

  def assignment(target: TypeReference, source: TypeReference): TypeUnificationGraph =
    new TypeUnificationGraph(Map.empty, Map.empty).addAssignment(target, source)

  case class Node(originType: TypeReference, equivalence: Seq[TypeReference])

  given Semigroup[Node] = (left: Node, right: Node) => Node(left.originType, left.equivalence ++ right.equivalence)

  given Semigroup[TypeUnificationGraph] = (left: TypeUnificationGraph, right: TypeUnificationGraph) =>
    TypeUnificationGraph(
      left.genericParameters ++ right.genericParameters, // These should be unique, so no merge
      (left.nodes.toSeq ++ right.nodes.toSeq).groupMapReduce(_._1)(_._2)(_ combine _)
    )
}
