package com.vanillasource.eliot.eliotc.processor

import cats.Monad
import cats.data.{Chain, EitherT, ReaderT, WriterT}
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.Sourced

object CompilerIO {
  case class Error(message: Sourced[String], description: Seq[String])

  private type EitherStage[T] = EitherT[IO, Chain[Error], T]
  private type WriterStage[T] = WriterT[EitherStage, Chain[Error], T]
  private type ReaderStage[T] = ReaderT[WriterStage, CompilationProcess, T]

  /** The effect all compiler processors run in. It is capable of accumulating errors, short-circuiting, and has access
    * to the CompilationProcess.
    */
  type CompilerIO[T] = ReaderStage[T]

  /** Returns the fact from the running compiler or short circuits.
    */
  def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): CompilerIO[V] =
    for {
      process <- ReaderT.ask[WriterStage, CompilationProcess]
      errors  <- currentErrors
      fact    <-
        ReaderT.liftF[WriterStage, CompilationProcess, V](
          WriterT.liftF(EitherT(process.getFact(key).map(_.toRight(errors))))
        )
    } yield fact

  /** Returns true if there are no errors accumulated in the CompilerIO.
    */
  def isClear: CompilerIO[Boolean] = currentErrors.map(_.isEmpty)

  /** Returns the currently accumulated errors.
    */
  def currentErrors: CompilerIO[Chain[Error]] =
    ReaderT.liftF[WriterStage, CompilationProcess, Chain[Error]](
      WriterT.liftF(Monad[EitherStage].unit).listen.map { case (_, errors) => errors }
    )

  /** Registers the fact, but only if the current compiler process is clean of errors!
    */
  def registerFact(value: CompilerFact): CompilerIO[Unit] =
    for {
      process <- ReaderT.ask[WriterStage, CompilationProcess]
      _       <- isClear.ifM(
                   ReaderT.liftF[WriterStage, CompilationProcess, Unit](
                     WriterT.liftF(EitherT.liftF(process.registerFact(value)))
                   ),
                   Monad[CompilerIO].unit
                 )
    } yield ()

}
