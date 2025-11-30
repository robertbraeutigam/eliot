package com.vanillasource.eliot.eliotc.main

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

final class FactGenerator(
    generator: CompilerProcessor,
    facts: Ref[IO, Map[CompilerFactKey[_], Deferred[IO, Option[CompilerFact]]]]
) extends CompilationProcess
    with Logging {
  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] = {
    for {
      _            <- debug(s"Getting (${key.getClass.getName}) $key")
      modifyResult <- modifyAtomicallyFor(key)
      _            <- (generator.generate(key)(using this) >> modifyResult._1.complete(None))
                        .whenA(modifyResult._2)
                        .start // Only if we are first
      result       <- modifyResult._1.get
      _            <-
        debug(
          s"${if (!modifyResult._2) "Cached" else (if (result.isDefined) "Returning" else "Failing")} (${key.getClass.getName}) $key"
        )
    } yield result.map(_.asInstanceOf[V])
  }

  override def registerFact(fact: CompilerFact): IO[Unit] = {
    debug(s"Inserting fact (${fact.getClass.getName}) with key ${fact.key()}") >>
      modifyAtomicallyFor(fact.key()).flatMap(_._1.complete(Some(fact)).void)
  }

  private def modifyAtomicallyFor(key: CompilerFactKey[_]): IO[(Deferred[IO, Option[CompilerFact]], Boolean)] =
    for {
      newValue <- Deferred[IO, Option[CompilerFact]]
      result   <- facts.modify { internalMap =>
                    internalMap.get(key) match
                      case Some(alreadyPresentValue) => (internalMap, (alreadyPresentValue, false))
                      case None                      => (internalMap + ((key, newValue)), (newValue, true))
                  }
    } yield result
}

object FactGenerator {
  def apply(generator: CompilerProcessor): IO[FactGenerator] =
    Ref.of[IO, Map[CompilerFactKey[_], Deferred[IO, Option[CompilerFact]]]](Map.empty).map { ref =>
      new FactGenerator(generator, ref)
    }
}
