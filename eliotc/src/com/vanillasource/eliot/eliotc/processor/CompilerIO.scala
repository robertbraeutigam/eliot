package com.vanillasource.eliot.eliotc.processor

import cats.Monad
import cats.data.{Chain, EitherT, ReaderT, StateT}
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.Sourced

object CompilerIO {
  case class Error(message: Sourced[String], description: Seq[String])

  private type EitherStage[T] = EitherT[IO, Chain[Error], T]
  private type StateStage[T] = StateT[EitherStage, Chain[Error], T]
  private type ReaderStage[T] = ReaderT[StateStage, CompilationProcess, T]

  /** The effect all compiler processors run in. It is capable of accumulating errors, short-circuiting, and has access
    * to the CompilationProcess.
    */
  type CompilerIO[T] = ReaderStage[T]

  /** Returns the fact from the running compiler as an Option, without aborting.
    */
  def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): CompilerIO[Option[V]] =
    for {
      process <- ReaderT.ask[StateStage, CompilationProcess]
      fact    <-
        ReaderT.liftF[StateStage, CompilationProcess, Option[V]](
          StateT.liftF(EitherT.liftF(process.getFact(key)))
        )
    } yield fact

  /** Returns the fact from the running compiler or short circuits.
    */
  def getFactOrAbort[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): CompilerIO[V] =
    for {
      process <- ReaderT.ask[StateStage, CompilationProcess]
      errors  <- currentErrors
      fact    <-
        ReaderT.liftF[StateStage, CompilationProcess, V](
          StateT.liftF(EitherT(process.getFact(key).map(_.toRight(errors))))
        )
    } yield fact

  /** Register an error.
    */
  def compilerError(message: Sourced[String], description: Seq[String] = Seq.empty): CompilerIO[Unit] =
    ReaderT.liftF[StateStage, CompilationProcess, Unit](
      StateT.modify[EitherStage, Chain[Error]](errors => errors :+ Error(message, description))
    )

  /** Returns true if there are no errors accumulated in the CompilerIO.
    */
  def isClear: CompilerIO[Boolean] = currentErrors.map(_.isEmpty)

  /** Returns the currently accumulated errors.
    */
  def currentErrors: CompilerIO[Chain[Error]] =
    ReaderT.liftF[StateStage, CompilationProcess, Chain[Error]](
      StateT.get[EitherStage, Chain[Error]]
    )

  /** Registers the fact, but only if the current compiler process is clean of errors!
    */
  def registerFactIfClear(value: CompilerFact): CompilerIO[Unit] =
    for {
      process <- ReaderT.ask[StateStage, CompilationProcess]
      _       <- isClear.ifM(
                   ReaderT.liftF[StateStage, CompilationProcess, Unit](
                     StateT.liftF(EitherT.liftF(process.registerFact(value)))
                   ),
                   Monad[CompilerIO].unit
                 )
    } yield ()

  /** Aborts the computation by copying errors from state into the Either's left side.
    */
  def abort[T]: CompilerIO[T] =
    for {
      errors <- currentErrors
      result <- ReaderT.liftF[StateStage, CompilationProcess, T](
                  StateT.liftF(EitherT.leftT[IO, T](errors))
                )
    } yield result

}
