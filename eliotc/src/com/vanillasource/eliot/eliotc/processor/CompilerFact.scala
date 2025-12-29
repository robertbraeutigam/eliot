package com.vanillasource.eliot.eliotc.processor

/** A single piece of information produced during the compilation process. Processors don't have an own state, i.e. all
  * processors are stateless. The only way processors keep track of the process is to consume and produce facts.
  */
trait CompilerFact {

  /** A key of a fact is the handle by which a fact can be accessed in the running engine.
    */
  def key(): CompilerFactKey[?]
}
