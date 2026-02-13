package com.vanillasource.eliot.eliotc.core.fact

import cats.Show

case class QualifiedName(name: String, qualifier: Qualifier)

object QualifiedName {
  given Show[QualifiedName] with {
    override def show(name: QualifiedName): String =
      name.name + (if (name.qualifier == Qualifier.Default) ("") else ("^" + name.qualifier.toString))
  }
}
