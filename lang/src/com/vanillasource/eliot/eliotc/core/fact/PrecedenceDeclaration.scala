package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.source.content.Sourced

case class PrecedenceDeclaration(relation: PrecedenceDeclaration.Relation, targets: Seq[Sourced[String]])

object PrecedenceDeclaration {
  sealed trait Relation

  object Relation {
    case object Above extends Relation
    case object Below extends Relation
    case object At    extends Relation
  }
}
