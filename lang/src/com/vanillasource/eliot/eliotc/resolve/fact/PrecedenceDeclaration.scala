package com.vanillasource.eliot.eliotc.resolve.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration.Relation

case class PrecedenceDeclaration(relation: Relation, targets: Seq[Sourced[ValueFQN]])
