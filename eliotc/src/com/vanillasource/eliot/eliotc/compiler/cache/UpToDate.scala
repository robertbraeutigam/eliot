package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The compiler-constant anchor for incremental compilation.
  *
  * A fact that is an *input-less compiler constant* — e.g. a `NativeBinding` from `SystemNativesProcessor`, which reads
  * no source and whose value is a non-serializable closure — would otherwise look like a *source leaf* (empty
  * dependencies) and be recomputed on every run. Taking a dependency on [[UpToDate]] instead turns it into a value-less
  * *non-leaf*: change-detection drills through to [[UpToDate]], a trivial leaf whose recomputed value always equals its
  * stored value, so the constant is proven unchanged without being materialised.
  *
  * This keeps the generator free of any "is this a pure constant?" special case. It is sound only because a compiler or
  * configuration change discards the whole cache (see `docs/incremental-compilation.md` §12–§13): [[UpToDate]] means
  * "constant for this compiler and configuration".
  */
case class UpToDate() extends CompilerFact {
  override def key(): CompilerFactKey[UpToDate] = UpToDate.Key()
}

object UpToDate {
  case class Key() extends CompilerFactKey[UpToDate]
}
