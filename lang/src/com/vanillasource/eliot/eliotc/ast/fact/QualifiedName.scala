package com.vanillasource.eliot.eliotc.ast.fact

import cats.{Eq, Show}

case class QualifiedName(name: String, qualifier: Qualifier)

object QualifiedName {
  given Show[QualifiedName] with {
    override def show(name: QualifiedName): String =
      name.name + (if (name.qualifier == Qualifier.Default) ("") else ("^" + name.qualifier.toString))
  }

  given Eq[QualifiedName] = Eq.fromUniversalEquals
}
