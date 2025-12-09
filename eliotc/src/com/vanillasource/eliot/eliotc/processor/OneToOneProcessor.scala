package com.vanillasource.eliot.eliotc.processor

import cats.syntax.all.*
import cats.{Applicative, Monad}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import scala.reflect.ClassTag

abstract class OneToOneProcessor[F[_]: Monad, V <: CompilerFact, I <: CompilerFactKey[V], O <: CompilerFactKey[_]](
    keyTransition: O => I
)(using ct: ClassTag[O])
    extends CompilerProcessor[F] {
  override def generate(factKey: CompilerFactKey[_])(using process: CompilationProcess[F]): F[Unit] =
    factKey match {
      case requestedKey: O =>
        val key = keyTransition(requestedKey)
        process.getFact(key).flatMap(_.traverse_(fact => generateFromKeyAndFact(requestedKey, fact)))
      case _               => Applicative[F].unit
    }

  def generateFromFact(fact: V)(using CompilationProcess[F]): F[Unit] = ???

  def generateFromKeyAndFact(key: O, fact: V)(using CompilationProcess[F]): F[Unit] =
    generateFromFact(fact)
}
