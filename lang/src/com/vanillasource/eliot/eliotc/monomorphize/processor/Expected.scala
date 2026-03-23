package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.eval.fact.Value

/** Bidirectional type direction for monomorphization.
  *
  * Replaces the implicit convention of using `Value.Type` as a sentinel for "unknown expected type". Makes the two
  * modes of type checking explicit:
  *   - [[Expected.Check]]: we know the expected type and push it down (e.g., lambda parameter type inference)
  *   - [[Expected.Synthesize]]: we compute the type bottom-up (e.g., literals, parameter references)
  */
sealed trait Expected

object Expected {
  case class Check(tpe: Value) extends Expected
  case object Synthesize extends Expected
}
