package com.vanillasource.eliot.eliotc

import com.vanillasource.eliot.eliotc.CompilerFact

/** Signal is a fact that doesn't have any content on top of its key. In other words, its mere existence or absence is
  * all the information it carries.
  */
trait CompilerSignal extends CompilerFact[CompilerSignal] {
  override def key(): CompilerSignal = this
}
