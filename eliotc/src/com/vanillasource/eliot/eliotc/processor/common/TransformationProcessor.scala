package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import scala.annotation.unused
import scala.reflect.ClassTag

abstract class TransformationProcessor[V <: CompilerFact, I <: CompilerFactKey[V], O <: CompilerFactKey[?]](
    keyTransition: O => I
)(using ct: ClassTag[O])
    extends SingleKeyTypeProcessor[O] {

  protected def processKey(requestedKey: O): CompilerIO[Unit] = {
    import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
    val key = keyTransition(requestedKey)
    getFactOrAbort(key).flatMap(fact => generateFromKeyAndFact(requestedKey, fact))
  }

  def generateFromFact(@unused fact: V): CompilerIO[Unit] = ???

  def generateFromKeyAndFact(@unused key: O, fact: V): CompilerIO[Unit] =
    generateFromFact(fact)
}
