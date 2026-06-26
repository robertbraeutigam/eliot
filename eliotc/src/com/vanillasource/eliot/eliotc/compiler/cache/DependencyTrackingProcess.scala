package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.{IO, Ref}
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey}

/** Wraps a [[CompilationProcess]] so that every fact read via `getFact` while `key` is being generated is recorded into
  * `directDependencies` as one of `key`'s direct dependencies.
  *
  * Recording happens **eagerly, on each read** — before the generated fact's [[cats.effect.Deferred]] is completed (a
  * processor reads its inputs before registering its output). This matters for correctness: a fact's dependency set must
  * be fully recorded by the time the fact becomes observable, otherwise a concurrent consumer (or the end-of-run cache
  * build) could see an incomplete set. A fresh wrapper is used per generation, so recording is per-fiber.
  *
  * This wrapper is also the explicit home of the **active fact-request chain**: built once per generation with `key`
  * and the `ancestors` it was requested under, it precomputes `chain = key :: ancestors` (innermost first). It serves
  * that chain as [[activeFactKeys]] (so the processor generating `key` can detect a cyclic request before it dead-locks
  * the fact cache) and threads it as the `ancestors` of every read it forwards, so the next generation down sees `key`
  * as its ancestor. The chain therefore flows explicitly through ordinary values — no ambient/fiber-local state.
  */
final class DependencyTrackingProcess(
    underlying: CompilationProcess,
    key: CompilerFactKey[?],
    directDependencies: Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]],
    ancestors: List[CompilerFactKey[?]]
) extends CompilationProcess {

  /** The active fact-request chain for the generation this wrapper tracks: `key` plus its ancestors, innermost first. */
  private val chain: List[CompilerFactKey[?]] = key :: ancestors

  /** Reads forward `chain` as their `ancestors`. The incoming `ancestors` argument is ignored: a processor reading a
    * fact does not know the chain — this wrapper is its source of truth for the generation of `key`.
    */
  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](
      dependency: K,
      ancestors: List[CompilerFactKey[?]]
  ): IO[Option[V]] =
    directDependencies.update(deps => deps.updated(key, deps.getOrElse(key, Set.empty) + dependency)) >>
      underlying.getFact(dependency, chain)

  override def registerFact(value: CompilerFact): IO[Unit] =
    underlying.registerFact(value)

  override def activeFactKeys: IO[List[CompilerFactKey[?]]] = IO.pure(chain)
}
