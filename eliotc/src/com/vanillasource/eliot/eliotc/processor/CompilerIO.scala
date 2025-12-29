package com.vanillasource.eliot.eliotc.processor

import cats.data.{Chain, OptionT, ReaderT, WriterT}
import cats.effect.IO
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
      fact    <- WriterT.liftF[OptionStage, Chain[Error], V](OptionT(process.getFact(key)))
    } yield fact

  /** Registers the fact, but only if the current compiler process is clean of errors!
    */
  def registerFact(value: CompilerFact)(using process: CompilationProcess): CompilerIO[Unit] =
    for {
      process <- ReaderT.ask[WriterStage, CompilationProcess]
    } yield ()

}
