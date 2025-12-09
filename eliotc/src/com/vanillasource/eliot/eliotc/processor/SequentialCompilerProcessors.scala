package com.vanillasource.eliot.eliotc.processor

import cats.Applicative
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class SequentialCompilerProcessors[F[_]: Applicative](processors: Seq[CompilerProcessor[F]])
    extends CompilerProcessor[F] {
  override def generate(factKey: CompilerFactKey[_])(using CompilationProcess[F]): F[Unit] =
    processors.traverse_(_.generate(factKey))
}
