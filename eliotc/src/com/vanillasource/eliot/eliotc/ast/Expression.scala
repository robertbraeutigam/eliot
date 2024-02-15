package com.vanillasource.eliot.eliotc.ast

import cats.Show
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[Token]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[Token])    extends Expression

  given Show[Tree[Expression]] = new Show[Tree[Expression]] {
    override def show(t: Tree[Expression]): String = t match
      case Tree.Empty()                                                      => "<empty tree>"
      case Tree.Node(IntegerLiteral(Sourced(_, _, value)), _)                => value.toString
      case Tree.Node(FunctionApplication(Sourced(_, _, value)), ns @ x :: _) =>
        s"${value.toString}(${ns.map(show).mkString(", ")})"
      case Tree.Node(FunctionApplication(Sourced(_, _, value)), _)           => value.toString
  }
}
