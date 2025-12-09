package com.vanillasource.eliot.eliotc.processor

import cats.Applicative
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class NullProcessor[F[_]: Applicative] extends CompilerProcessor[F] {
  override def generate(factKey: CompilerFactKey[_])(using CompilationProcess[F]): F[Unit] = Applicative[F].unit
}
