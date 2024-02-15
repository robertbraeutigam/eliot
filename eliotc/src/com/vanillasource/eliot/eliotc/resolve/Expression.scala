package com.vanillasource.eliot.eliotc.resolve

import cats.Show
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.source.Sourced

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[FunctionFQN]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])         extends Expression

  given Show[Tree[Expression]] = new Show[Tree[Expression]] {
    override def show(t: Tree[Expression]): String = t match
      case Tree.Empty()                                                      => "<empty tree>"
      case Tree.Node(IntegerLiteral(Sourced(_, _, value)), _)                => value.toString()
      case Tree.Node(FunctionApplication(Sourced(_, _, value)), ns @ x :: _) =>
        s"${value.show}(${ns.map(show).mkString(", ")})"
      case Tree.Node(FunctionApplication(Sourced(_, _, value)), _)           => value.show
  }

}
