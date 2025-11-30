package com.vanillasource.eliot.eliotc.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import scala.reflect.ClassTag

abstract class OneToOneProcessor[V <: CompilerFact, I <: CompilerFactKey[V], O <: CompilerFactKey[_]](
    keyTransition: O => I
)(using ct: ClassTag[O])
    extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[_])(using process: CompilationProcess): IO[Unit] =
    factKey match {
      case requestedKey: O =>
        val key = keyTransition(requestedKey)
        process.getFact(key).flatMap(_.traverse_(fact => generateFromFact(fact)))
      case _               => IO.unit
    }

  def generateFromFact(fact: V)(using CompilationProcess): IO[Unit]
}
