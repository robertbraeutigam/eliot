package com.vanillasource.eliot.eliotc.module.fact

import cats.{Eq, Show}

case class QualifiedName(name: String, qualifier: Qualifier)

object QualifiedName {
  given Show[QualifiedName] with {
    override def show(name: QualifiedName): String =
      if (name.qualifier == Qualifier.Default) name.name
      else s"${name.name}^${Show[Qualifier].show(name.qualifier)}"
  }

  given Eq[QualifiedName] = Eq.fromUniversalEquals
}
