package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
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
](using ct: ClassTag[OutputKey])
    extends SingleFactProcessor[OutputFact, OutputKey] {

  protected def getInputKey(outputKey: OutputKey): InputKey

  protected def generateSingleFact(requestedKey: OutputKey): CompilerIO[OutputFact] =
    getFactOrAbort(getInputKey(requestedKey)).flatMap(fact => generateFromKeyAndFact(requestedKey, fact))

  protected def generateFromKeyAndFact(key: OutputKey, fact: InputFact): CompilerIO[OutputFact]
}
