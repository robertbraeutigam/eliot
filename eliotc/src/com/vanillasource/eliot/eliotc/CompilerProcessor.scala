package com.vanillasource.eliot.eliotc

import cats.effect.IO

/** All processors of the compiler must implement this trait to generate facts. When someone requests a fact from the
  * engine which is not yet present, the engine will ask processors to generate the fact through this interface.
  */
trait CompilerProcessor[F[_]] {

  /** Generate the fact with the given key, if able. Otherwise, do nothing.
    */
  def generate(factKey: CompilerFactKey[_])(using CompilationProcess[F]): F[Unit]
}
