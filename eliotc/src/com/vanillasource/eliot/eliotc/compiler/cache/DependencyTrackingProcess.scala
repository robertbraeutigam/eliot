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
  */
final class DependencyTrackingProcess(
    underlying: CompilationProcess,
    key: CompilerFactKey[?],
    directDependencies: Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]]
) extends CompilationProcess {

  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](dependency: K): IO[Option[V]] =
    directDependencies.update(deps => deps.updated(key, deps.getOrElse(key, Set.empty) + dependency)) >>
      underlying.getFact(dependency)

  override def registerFact(value: CompilerFact): IO[Unit] =
    underlying.registerFact(value)

  override def activeFactKeys: IO[List[CompilerFactKey[?]]] =
    underlying.activeFactKeys
}
