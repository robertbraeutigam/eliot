package com.vanillasource.eliot.eliotc.source.error

import cats.data.{IndexedStateT, OptionT, StateT}
import cats.{Applicative, Monad}
import cats.effect.std.Console
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.source.error.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.util.CatsOps.*

import java.io.File

object CompilationF {
  type CompilationF[F[_], T] = StateT[[X] =>> OptionT[F, X], Boolean, T]

  def compilationError[F[_]: Monad]: CompilationF[F, Unit] = StateT.set(false)

  def compilationAbort[F[_]: Monad, T]: CompilationF[F, T] = StateT.liftF(OptionT.none)

  extension [F[_]: Monad, A](value: F[Option[A]]) {
    def liftOptionToCompilation: CompilationF[F, A] = StateT.liftF(OptionT(value))
  }

  extension [F[_]: Monad, A](value: F[A]) {
    def liftToCompilation: CompilationF[F, A] = StateT.liftF(value.map(Some.apply).toOptionT)

    def liftIfNoErrors: CompilationF[F, Unit] = value.liftToCompilation.ifNoErrors
  }

  extension [F[_]: Monad, A](value: CompilationF[F, A]) {
    def runCompilation_(): F[Unit] = value.run(true).value.void

    def ifNoErrors: CompilationF[F, Unit] =
      StateT.get.ifM(value.as(()), StateT[[X] =>> OptionT[F, X], Boolean, Unit].void)
  }

  def compilerError[F[_]: {Monad, Console}](message: Sourced[String], description: Seq[String] = Seq.empty)(using
      process: CompilationProcess[F]
  ): CompilationF[F, Unit] =
    registerCompilerError(message, description).liftToCompilation >> compilationError

  def compilerError[F[_]: {Monad, Console}](file: File, message: String)(using
      process: CompilationProcess[F]
  ): CompilationF[F, Unit] =
    registerCompilerError(file, message).liftToCompilation >> compilationError

  def compilerAbort[F[_]: {Monad, Console}, T](message: Sourced[String])(using
      process: CompilationProcess[F]
  ): CompilationF[F, T] =
    compilerError(message) >> compilationAbort

  def compilerAbort[F[_]: {Monad, Console}, T](file: File, message: String)(using
      process: CompilationProcess[F]
  ): CompilationF[F, T] =
    compilerError(file, message) >> compilationAbort
}
