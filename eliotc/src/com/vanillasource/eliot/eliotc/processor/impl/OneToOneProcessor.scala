package com.vanillasource.eliot.eliotc.processor.impl

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilationProcess.getFact
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import scala.annotation.unused
import scala.reflect.ClassTag

abstract class OneToOneProcessor[V <: CompilerFact, I <: CompilerFactKey[V], O <: CompilerFactKey[?]](
    keyTransition: O => I
)(using ct: ClassTag[O])
    extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess): IO[Unit] =
    factKey match {
      case requestedKey: O =>
        val key = keyTransition(requestedKey)
        getFact(key).flatMap(_.traverse_(fact => generateFromKeyAndFact(requestedKey, fact)))
      case _               => IO.unit
    }

  def generateFromFact(@unused fact: V)(using @unused cp: CompilationProcess): IO[Unit] =
    IO.raiseError(new UnsupportedOperationException("Not implemented"))

  def generateFromKeyAndFact(@unused key: O, fact: V)(using CompilationProcess): IO[Unit] =
    generateFromFact(fact)
}
