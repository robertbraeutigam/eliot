package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration.Relation

case class PrecedenceDeclaration(relation: Relation, targets: Seq[Sourced[String]])
