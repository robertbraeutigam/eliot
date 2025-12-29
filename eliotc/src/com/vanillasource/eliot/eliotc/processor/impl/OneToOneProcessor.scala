package com.vanillasource.eliot.eliotc.processor.impl

import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, CompilerProcessor}

import scala.annotation.unused
import scala.reflect.ClassTag

abstract class OneToOneProcessor[V <: CompilerFact, I <: CompilerFactKey[V], O <: CompilerFactKey[?]](
    keyTransition: O => I
)(using ct: ClassTag[O])
    extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    factKey match {
      case requestedKey: O =>
        val key = keyTransition(requestedKey)
        getFactOrAbort(key).flatMap(fact => generateFromKeyAndFact(requestedKey, fact))
      case _               => Monad[CompilerIO].unit
    }

  def generateFromFact(@unused fact: V): CompilerIO[Unit] = ???

  def generateFromKeyAndFact(@unused key: O, fact: V): CompilerIO[Unit] =
    generateFromFact(fact)
}
