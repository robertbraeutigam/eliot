package com.vanillasource.eliot.eliotc.processor

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

  /** The keys of the fact computations currently in progress on this request chain (innermost first) — the ancestors
    * of the fact being generated right now. A processor reads this to detect a cyclic / non-stabilising fact-request
    * chain: requesting a key that is already an ancestor would dead-lock the [[Deferred]]-based fact cache (the
    * in-progress computation would end up waiting on itself), so a processor can instead report a specific error. The
    * default is empty (no tracking); [[com.vanillasource.eliot.eliotc.compiler.IncrementalFactGenerator]] maintains the
    * real chain.
    */
  def activeFactKeys: IO[List[CompilerFactKey[?]]] = IO.pure(Nil)
}
