package com.vanillasource.eliot.eliotc.processor

trait CompilerFactKey[V]

/** Type-level function to extract the fact type from a key type */
type FactOf[K <: CompilerFactKey[?]] = K match {
  case CompilerFactKey[f] => f
}
