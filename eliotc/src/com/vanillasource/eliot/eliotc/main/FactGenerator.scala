package com.vanillasource.eliot.eliotc.main

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

final class FactGenerator(
    generator: CompilerProcessor,
    facts: Ref[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]]
) extends CompilationProcess
    with Logging {
  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] = {
    for {
      _            <- debug[IO](s"Getting (${key.getClass.getName}) $key")
      modifyResult <- modifyAtomicallyFor(key)
      _            <- (generator
                        .generate(key)(using this)
                        .recoverWith(t => error[IO](s"Getting (${key.getClass.getName}) $key failed.", t)) >>
                        modifyResult._1.complete(None)).start
                        .whenA(modifyResult._2) // Only if we are first
      result       <- modifyResult._1.get
      _            <-
        debug[IO](
          s"${if (!modifyResult._2) "Cached" else if (result.isDefined) "Returning" else "Failing"} (${key.getClass.getName}) $key"
        )
    } yield result.map(_.asInstanceOf[V])
  }

  override def registerFact(fact: CompilerFact): IO[Unit] = {
    debug[IO](s"Inserting fact (${fact.getClass.getName}) with key ${fact.key()}") >>
      modifyAtomicallyFor(fact.key()).flatMap(_._1.complete(Some(fact)).void)
  }

  private def modifyAtomicallyFor(
      key: CompilerFactKey[?]
  ): IO[(Deferred[IO, Option[CompilerFact]], Boolean)] =
    for {
      newValue <- Deferred[IO, Option[CompilerFact]]
      result   <- facts.modify { internalMap =>
                    internalMap.get(key) match
                      case Some(alreadyPresentValue) => (internalMap, (alreadyPresentValue, false))
                      case None                      => (internalMap.updated(key, newValue), (newValue, true))
                  }
    } yield result

  def currentFacts(): IO[Map[CompilerFactKey[?], CompilerFact]] =
    for {
      currentMap <- facts.get
      facts      <- currentMap.values.toSeq.traverse(_.tryGet.map(_.flatten)).map(_.flatten)
    } yield facts.map(v => v.key() -> v).toMap
}

object FactGenerator {
  def create(generator: CompilerProcessor): IO[FactGenerator] =
    Ref.of[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]](Map.empty).map { ref =>
      new FactGenerator(generator, ref)
    }
}
