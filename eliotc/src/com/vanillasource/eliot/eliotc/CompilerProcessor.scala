package com.vanillasource.eliot.eliotc

import cats.effect.IO

/** All processors of the compiler must implement this trait to process facts. Processors are called with every single
  * fact that is produced and must decide themselves whether they want to act on it or not.
  */
trait CompilerProcessor {

  /** Process a fact. All processors will be be called with all the facts produced in the engine, the processor must
    * ignore facts it doesn't need.
    */
  def process(fact: CompilerFact)(using CompilationProcess): IO[Unit]
}
