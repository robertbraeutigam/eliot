package com.vanillasource.eliot.eliotc

import cats.effect.IO
import cats.syntax.all.*

/** Passed into processors to access the currently running compilation process. Note, that processors can not have state
  * and are only allowed to interact with the current compilation process by consuming or producing facts that are
  * shared with other processors.
  */
trait CompilationProcess {

  /** Get a fact from the currently running compilation process. If the fact is available, it is returned immediately.
    * If the generation of the fact previously failed, this call will return with None. Otherwise, the processors will
    * be asked to generate this fact and this call will block until that process ends. If this call returns with None,
    * that means this fact will never be produced.
    */
  def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]]

  def registerFact(value: CompilerFact): IO[Unit]

  def registerFacts(values: Seq[CompilerFact]): IO[Unit] =
    values.map(registerFact).sequence_
}

/** Convenience functions that use implicit compilation process. */
object CompilationProcess {
  def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K)(using process: CompilationProcess): IO[Option[V]] =
    process.getFact(key)

  def registerFact(value: CompilerFact)(using process: CompilationProcess): IO[Unit] =
    process.registerFact(value)
}
