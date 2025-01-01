package com.vanillasource.eliot.eliotc.source

import cats.data.{IndexedStateT, StateT}
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError

import java.io.File

object CompilationIO {
  type CompilationIO[T] = StateT[IO, Boolean, T]

  val compilationError: CompilationIO[Unit] = StateT.set[IO, Boolean](false)

  extension [A](value: IO[A]) {
    def liftToCompilationIO: CompilationIO[A] = StateT.liftF[IO, Boolean, A](value)
  }

  extension [A](value: CompilationIO[A]) {
    def runCompilation(): IO[(Boolean, A)] = value.run(true)

    def runIfSuccessful(block: A => IO[Unit]): IO[Unit] = runCompilation().flatMap {
      case (true, value) => block(value)
      case _             => IO.unit
    }
  }

  def compilerError(message: Sourced[String])(using process: CompilationProcess): CompilationIO[Unit] =
    registerCompilerError(message).liftToCompilationIO >> compilationError

  def compilerError(file: File, message: String)(using process: CompilationProcess): CompilationIO[Unit] =
    registerCompilerError(file, message).liftToCompilationIO >> compilationError

}
