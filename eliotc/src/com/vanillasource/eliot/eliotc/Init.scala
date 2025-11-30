package com.vanillasource.eliot.eliotc

/** Inserted when the compiler is started as the first and only automatically inserted fact. This fact can be used to
  * bootstrap initial processors.
  */
case class Init() extends CompilerFact {
  override def key(): CompilerFactKey[Init] = Init.Key()
}

object Init {
  case class Key() extends CompilerFactKey[Init]
}
