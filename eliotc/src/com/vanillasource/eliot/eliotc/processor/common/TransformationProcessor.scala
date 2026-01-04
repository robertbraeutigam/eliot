package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import scala.annotation.unused
import scala.reflect.ClassTag
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

/** When asked to generate a certain key type, it gets another fact based on that key and transforms that fact to
  * generate the new one. Transformation processors must only generate one fact, but may get more facts to supplement
  * the transformation of the "main" fact being transformed.
  */
abstract class TransformationProcessor[
    OutputFact <: CompilerFact,
    InputFact <: CompilerFact,
    InputKey <: CompilerFactKey[InputFact],
    OutputKey <: CompilerFactKey[OutputFact]
](keyTransition: OutputKey => InputKey)(using ct: ClassTag[OutputKey])
    extends SingleFactProcessor[OutputFact, OutputKey] {

  protected def generateSingleFact(requestedKey: OutputKey): CompilerIO[OutputFact] =
    getFactOrAbort(keyTransition(requestedKey)).flatMap(fact => generateFromKeyAndFact(requestedKey, fact))

  def generateFromFact(@unused fact: InputFact): CompilerIO[OutputFact] = ???

  def generateFromKeyAndFact(@unused key: OutputKey, fact: InputFact): CompilerIO[OutputFact] =
    generateFromFact(fact)
}
