package com.vanillasource.eliot.eliotc.processor

import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO

/** All processors of the compiler must implement this trait to generate facts. When someone requests a fact from the
  * engine which is not yet present, the engine will ask processors to generate the fact through this interface.
  */
trait CompilerProcessor {

  /** Generate the fact with the given key, if able. Otherwise, do nothing.
    */
  def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit]

  /** Wrap this processor with the given wrapper function. Intended to be implemented by container type processors.
    */
  def wrapWith(wrapper: CompilerProcessor => CompilerProcessor): CompilerProcessor = wrapper(this)

  /** Whether this processor could generate the given key at all — a cheap pre-test, so a processor that has nothing to
    * do with a key is not invoked in the first place. The engine offers every requested key to every processor, so this
    * runs once per (key, processor) pair, hundreds of thousands of times in a compilation; invoking a processor to have
    * it decline is not free, because each invocation is error-isolated on its own [[CompilerIO]] context.
    *
    * The default answers "possibly", which is always safe: the processor is then invoked and decides as before. Only
    * override it with a test that cannot say "no" to a key the processor would in fact have generated — a wrong "no" is
    * a fact that silently never gets produced. Note that a processor which *wraps* another must delegate this, or the
    * wrapped processor's answer is lost and the pre-test degrades to "possibly" for everything.
    */
  def handles(factKey: CompilerFactKey[?]): Boolean = true
}
