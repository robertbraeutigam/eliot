package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, FactOf}
import scala.reflect.ClassTag
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

/** When asked to generate a certain key type, it gets another fact based on that key and transforms that fact to
  * generate the new one. Transformation processors must only generate one fact, but may get more facts to supplement
  * the transformation of the "main" fact being transformed.
  *
  * Type parameters have been reduced from 4 to 2 by inferring fact types from key types using the FactOf match type.
  * InputFact = FactOf[InputKey] and OutputFact = FactOf[OutputKey] are computed at compile time.
  *
  * @tparam InputKey
  *   The key type for the input fact being transformed
  * @tparam OutputKey
  *   The key type for the output fact being generated
  * @param getInputKey
  *   Function to derive the input key from the output key (e.g., `_.file` or `outputKey => InputKey(outputKey.field)`)
  */
abstract class TransformationProcessor[
    InputKey <: CompilerFactKey[?],
    OutputKey <: CompilerFactKey[?]
](getInputKey: OutputKey => InputKey)(using ct: ClassTag[OutputKey])
    extends SingleKeyTypeProcessor[OutputKey] {

  type InputFact  = FactOf[InputKey]
  type OutputFact = FactOf[OutputKey]

  override protected def generateFact(requestedKey: OutputKey): CompilerIO[Unit] =
    for {
      inputFact  <- getFactOrAbort(getInputKey(requestedKey).asInstanceOf[CompilerFactKey[CompilerFact]])
      outputFact <- generateFromKeyAndFact(requestedKey, inputFact.asInstanceOf[InputFact])
      _          <- registerFactIfClear(outputFact.asInstanceOf[CompilerFact])
    } yield ()

  protected def generateFromKeyAndFact(key: OutputKey, fact: InputFact): CompilerIO[OutputFact]
}
