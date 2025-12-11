package com.vanillasource.eliot.eliotc.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import scala.annotation.unused
import scala.reflect.ClassTag

abstract class OneToOneProcessor[V <: CompilerFact, I <: CompilerFactKey[V], O <: CompilerFactKey[?]](
    keyTransition: O => I
)(using ct: ClassTag[O])
    extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?])(using process: CompilationProcess): IO[Unit] =
    factKey match {
      case requestedKey: O =>
        val key = keyTransition(requestedKey)
        process.getFact(key).flatMap(_.traverse_(fact => generateFromKeyAndFact(requestedKey, fact)))
      case _               => IO.unit
    }

  def generateFromFact(@unused fact: V)(using @unused cp: CompilationProcess): IO[Unit] =
    IO.raiseError(new UnsupportedOperationException("Not implemented"))

  def generateFromKeyAndFact(@unused key: O, fact: V)(using CompilationProcess): IO[Unit] =
    generateFromFact(fact)
}
