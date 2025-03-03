package com.vanillasource.eliot.eliotc.source

import cats.data.{IndexedStateT, OptionT, StateT}
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError

import java.io.File
import com.vanillasource.util.CatsOps.*

object CompilationIO {
  private type OptionIOT[T] = OptionT[IO, T]
  type CompilationIO[T]     = StateT[OptionIOT, Boolean, T]

  val compilationError: CompilationIO[Unit] = StateT.set[OptionIOT, Boolean](false)

  val compilationAbort: CompilationIO[Unit] = StateT.liftF(OptionT.none)

  extension [A](value: IO[A]) {
    def liftToCompilationIO: CompilationIO[A] = StateT.liftF[OptionIOT, Boolean, A](value.map(Some.apply).toOptionT)

    def liftIfNoErrors: CompilationIO[Unit] = value.liftToCompilationIO.ifNoErrors
  }

  extension [A](value: CompilationIO[A]) {
    def runCompilation_(): IO[Unit] = value.run(true).value.void

    def ifNoErrors: CompilationIO[Unit] =
      StateT.get[OptionIOT, Boolean].ifM(value.void, IO.unit.liftToCompilationIO)
  }

  def compilerError(message: Sourced[String])(using process: CompilationProcess): CompilationIO[Unit] =
    registerCompilerError(message).liftToCompilationIO >> compilationError

  def compilerError(file: File, message: String)(using process: CompilationProcess): CompilationIO[Unit] =
    registerCompilerError(file, message).liftToCompilationIO >> compilationError

  def compilerAbort(message: Sourced[String])(using process: CompilationProcess): CompilationIO[Unit] =
    compilerError(message) >> compilationAbort

  def compilerAbort(file: File, message: String)(using process: CompilationProcess): CompilationIO[Unit] =
    compilerError(file, message) >> compilationAbort
}
