package com.vanillasource.eliot.eliotc.source.error

import cats.data.{IndexedStateT, OptionT, StateT}
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.Sourced
import com.vanillasource.eliot.eliotc.processor.CompilationProcess
import com.vanillasource.eliot.eliotc.source.error.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.util.CatsOps.*

import java.io.File

object CompilationIO {
  private type OptionIOT[T] = OptionT[IO, T]
  type CompilationIO[T]     = StateT[OptionIOT, Boolean, T]

  val compilationError: CompilationIO[Unit] = StateT.set[OptionIOT, Boolean](false)

  def compilationAbort[T]: CompilationIO[T] = StateT.liftF(OptionT.none)

  extension [A](value: IO[Option[A]]) {
    def liftOptionToCompilationIO: CompilationIO[A] = StateT.liftF[OptionIOT, Boolean, A](OptionT(value))
  }

  extension [A](value: IO[A]) {
    def liftToCompilationIO: CompilationIO[A] = StateT.liftF[OptionIOT, Boolean, A](value.map(Some.apply).toOptionT)

    def liftIfNoErrors: CompilationIO[Unit] = value.liftToCompilationIO.ifNoErrors
  }

  extension [A](value: CompilationIO[A]) {
    def runCompilation_(): IO[Unit] = value.run(true).value.void

    def ifNoErrors: CompilationIO[Unit] =
      StateT.get[OptionIOT, Boolean].ifM(value.void, IO.unit.liftToCompilationIO)
  }

  def compilerError(message: Sourced[String], description: Seq[String] = Seq.empty)(using
      CompilationProcess
  ): CompilationIO[Unit] =
    registerCompilerError(message, description).liftToCompilationIO >> compilationError

  def compilerError(file: File, message: String)(using CompilationProcess): CompilationIO[Unit] =
    registerCompilerError(file, message).liftToCompilationIO >> compilationError

  def compilerAbort[T](message: Sourced[String])(using CompilationProcess): CompilationIO[T] =
    compilerError(message) >> compilationAbort

  def compilerAbort[T](file: File, message: String)(using CompilationProcess): CompilationIO[T] =
    compilerError(file, message) >> compilationAbort
}
