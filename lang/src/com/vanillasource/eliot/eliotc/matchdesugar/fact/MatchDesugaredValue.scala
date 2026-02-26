package com.vanillasource.eliot.eliotc.matchdesugar.fact

import com.vanillasource.eliot.eliotc.ast.fact.Fixity
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, PrecedenceDeclaration, QualifiedName, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class MatchDesugaredValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[Expression]],
    typeStack: Sourced[TypeStack[Expression]],
    paramConstraints: Map[String, Seq[ResolvedValue.ResolvedAbilityConstraint]] = Map.empty,
    fixity: Fixity = Fixity.Application,
    precedence: Seq[PrecedenceDeclaration] = Seq.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[MatchDesugaredValue] = MatchDesugaredValue.Key(vfqn)
}

object MatchDesugaredValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[MatchDesugaredValue]
}
