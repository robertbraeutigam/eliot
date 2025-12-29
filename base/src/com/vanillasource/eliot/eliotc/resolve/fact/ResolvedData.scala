package com.vanillasource.eliot.eliotc.resolve.fact

import com.vanillasource.eliot.eliotc.module.fact.TypeFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class ResolvedData(tfqn: TypeFQN, definition: DataDefinition) extends CompilerFact {
  override def key(): CompilerFactKey[ResolvedData] = ResolvedData.Key(tfqn)
}

object ResolvedData {
  case class Key(tfqn: TypeFQN) extends CompilerFactKey[ResolvedData]
}
