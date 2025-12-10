package com.vanillasource.eliot.eliotc

import cats.Applicative
import cats.syntax.all.*

/** Passed into processors to access the currently running compilation process. Note, that processors can not have state
  * and are only allowed to interact with the current compilation process by consuming or producing facts that are
  * shared with other processors.
  */
trait CompilationProcess[F[_]] {

  /** Get a fact from the currently running compilation process. If the fact is available, it is returned immediately.
    * If the fact is not available, this call will block until the fact for the given key becomes available. If the fact
    * will not become available ever, because there are no more actively running processors, this will return None.
    * Important note: A processor is only allowed to wait on a single fact at any given time. That is, it is not allowed
    * to parallel wait on multiple calls.
    */
  def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): F[Option[V]]

  def registerFact(value: CompilerFact): F[Unit]

  def registerFacts(values: Seq[CompilerFact])(using Applicative[F]): F[Unit] =
    values.map(registerFact).sequence_
}
