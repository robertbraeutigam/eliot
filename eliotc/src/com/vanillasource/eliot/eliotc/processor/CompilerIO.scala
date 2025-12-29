package com.vanillasource.eliot.eliotc.processor

import cats.Monad
import cats.data.{Chain, OptionT, ReaderT, WriterT}
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.Sourced

object CompilerIO {
  case class Error(message: Sourced[String], description: Seq[String])

  private type OptionStage[T] = OptionT[IO, T]
  private type WriterStage[T] = WriterT[OptionStage, Chain[Error], T]

  /** The effect all compiler processors run in. It is capable of accumulating errors, short-circuiting, and has access
    * to the CompilationProcess.
    */
  type CompilerIO[T] = ReaderT[WriterStage, CompilationProcess, T]

  /** Returns the fact from the running compiler or short circuits.
    */
  def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): CompilerIO[V] =
    for {
      process <- ReaderT.ask[WriterStage, CompilationProcess]
      fact    <- ReaderT.liftF[WriterStage, CompilationProcess, V](WriterT.liftF(OptionT(process.getFact(key))))
    } yield fact

  /** Returns true if there are no errors accumulated in the CompilerIO.
    */
  def isClear: CompilerIO[Boolean] =
    ReaderT.liftF[WriterStage, CompilationProcess, Boolean](
      WriterT.liftF(OptionT.liftF(IO.unit)).listen.map { case (_, errors) => errors.isEmpty }
    )

  /** Registers the fact, but only if the current compiler process is clean of errors!
    */
  def registerFact(value: CompilerFact): CompilerIO[Unit] =
    for {
      process <- ReaderT.ask[WriterStage, CompilationProcess]
      _       <- isClear.ifM(
                   ReaderT.liftF[WriterStage, CompilationProcess, Unit](
                     WriterT.liftF(OptionT.liftF(process.registerFact(value)))
                   ),
                   Monad[CompilerIO].unit
                 )
    } yield ()

}
