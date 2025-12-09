package com.vanillasource.eliot.eliotc.main

import cats.effect.implicits.genSpawnOps
import cats.{Applicative, Monad}
import cats.effect.{Async, Concurrent, Deferred, Ref, Spawn, Sync}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.feedback.Logging.Log
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

final class FactGenerator[F[_]: Async: Log](
    generator: CompilerProcessor[F],
    facts: Ref[F, Map[CompilerFactKey[_], Deferred[F, Option[CompilerFact]]]]
) extends CompilationProcess[F]
    with Logging {
  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): F[Option[V]] = {
    for {
      _            <- debug[F](s"Getting (${key.getClass.getName}) $key")
      modifyResult <- modifyAtomicallyFor(key)
      _            <- generator.generate(key)(using this) >> modifyResult._1
                        .complete(None)
                        .whenA(modifyResult._2)
                        .start // Only if we are first
      result       <- modifyResult._1.get
      _            <-
        debug(
          s"${if (!modifyResult._2) "Cached" else if (result.isDefined) "Returning" else "Failing"} (${key.getClass.getName}) $key"
        )
    } yield result.map(_.asInstanceOf[V])
  }

  override def registerFact(fact: CompilerFact): F[Unit] = {
    debug[F](s"Inserting fact (${fact.getClass.getName}) with key ${fact.key()}") >>
      modifyAtomicallyFor(fact.key()).flatMap(_._1.complete(Some(fact)).void)
  }

  private def modifyAtomicallyFor(
      key: CompilerFactKey[_]
  ): F[(Deferred[F, Option[CompilerFact]], Boolean)] =
    for {
      newValue <- Deferred[F, Option[CompilerFact]]
      result   <- facts.modify { internalMap =>
                    internalMap.get(key) match
                      case Some(alreadyPresentValue) => (internalMap, (alreadyPresentValue, false))
                      case None                      => (internalMap + ((key, newValue)), (newValue, true))
                  }
    } yield result
}

object FactGenerator {
  def create[F[_]: Async](generator: CompilerProcessor[F]): F[FactGenerator[F]] =
    Ref.of[F, Map[CompilerFactKey[_], Deferred[F, Option[CompilerFact]]]](Map.empty).map { ref =>
      new FactGenerator(generator, ref)
    }
}
