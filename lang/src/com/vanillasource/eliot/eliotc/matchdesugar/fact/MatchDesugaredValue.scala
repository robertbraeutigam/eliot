package com.vanillasource.eliot.eliotc.matchdesugar.fact

import com.vanillasource.eliot.eliotc.ast.fact.Fixity
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, PrecedenceDeclaration, QualifiedName, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class MatchDesugaredValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[MatchDesugaredExpression]],
    typeStack: Sourced[TypeStack[MatchDesugaredExpression]],
    paramConstraints: Map[String, Seq[MatchDesugaredValue.ResolvedAbilityConstraint]] = Map.empty,
    fixity: Fixity = Fixity.Application,
    precedence: Seq[PrecedenceDeclaration] = Seq.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[MatchDesugaredValue] = MatchDesugaredValue.Key(vfqn)
}

object MatchDesugaredValue {
  case class ResolvedAbilityConstraint(abilityFQN: AbilityFQN, typeArgs: Seq[MatchDesugaredExpression])

  case class Key(vfqn: ValueFQN) extends CompilerFactKey[MatchDesugaredValue]
}
