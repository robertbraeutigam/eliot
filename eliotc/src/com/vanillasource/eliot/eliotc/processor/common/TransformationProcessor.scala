package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import scala.annotation.unused
import scala.reflect.ClassTag
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

abstract class TransformationProcessor[V <: CompilerFact, I <: CompilerFactKey[V], O <: CompilerFactKey[?]](
    keyTransition: O => I
)(using ct: ClassTag[O])
    extends SingleKeyTypeProcessor[O] {

  protected def generateFact(requestedKey: O): CompilerIO[Unit] =
    getFactOrAbort(keyTransition(requestedKey)).flatMap(fact => generateFromKeyAndFact(requestedKey, fact))

  def generateFromFact(@unused fact: V): CompilerIO[Unit] = ???

  def generateFromKeyAndFact(@unused key: O, fact: V): CompilerIO[Unit] =
    generateFromFact(fact)
}
