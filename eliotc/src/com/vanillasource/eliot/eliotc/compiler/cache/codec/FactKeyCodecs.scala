package com.vanillasource.eliot.eliotc.compiler.cache.codec

import com.vanillasource.eliot.eliotc.plugin.Configuration
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

import scala.reflect.ClassTag

/** The one place the cache needs a **type tag**: how to decode a fact *key* (`docs/incremental-compilation.md` §16,
  * §17).
  *
  * Values need no tag — a fact's value is decoded by the codec its own key states, and everything below it
  * structurally, by a codec that already knows what it is reading. A key has nothing above it to say what it is: an
  * entry's dependencies are keys, and the validation walk must materialise them to ask the engine to recompute what
  * they identify. So keys, and only keys, are stored beside a name that maps back to a codec.
  *
  * Two properties, and they are why this is a registry rather than a member on `CompilerFactKey`:
  *
  *   - **Stable across runs.** The name is the key class's own, so adding a fact type shifts nothing; an unknown name
  *     read back is a cache miss, never a mis-decode. (`CacheFingerprint.compiler` already discards the whole cache on
  *     any compiler change, so a name needs to survive only within one compiler build.)
  *   - **Complete, or a hard error at save.** `CompilerFactKey.valueCodec` is compile-time-complete for the value
  *     because every key states it. Decoding cannot be stated that way — the reader has no key yet — so this is a
  *     runtime map and needs its own guard: a key type absent from it must fail loudly when written, never leave bytes
  *     nothing can read back.
  */
object FactKeyCodecs {

  /** Key class name to the codec that reads it. Assembled per layer (`CoreFactCodecs`, `LangFactCodecs`,
    * `JvmFactCodecs`, …) and unioned by whoever opens a store.
    */
  type Registry = Map[String, FactCodec[CompilerFactKey[?]]]

  /** Where each layer contributes its own registrations, in `configure()`, as the platform layers contribute run
    * boundaries and native labels. The store is opened from the effective configuration, so it sees the union of
    * whatever plugins are active — which is exactly the set of fact types that build can produce.
    */
  // Opaque to the cache identity: a deterministic function of the active plugin set, which is fixed within a compiler
  // build and already covered by the compiler fingerprint.
  val configKey: Configuration.Key[Registry] = Configuration.opaqueKey("factKeyCodecs")

  /** Register one key type under its own class name. */
  def of[K <: CompilerFactKey[?]](codec: FactCodec[K])(using
      tag: ClassTag[K]
  ): (String, FactCodec[CompilerFactKey[?]]) =
    tag.runtimeClass.getName -> codec.asInstanceOf[FactCodec[CompilerFactKey[?]]]

  /** The name a key is stored under, and looked back up by. */
  def nameOf(key: CompilerFactKey[?]): String = key.getClass.getName
}
