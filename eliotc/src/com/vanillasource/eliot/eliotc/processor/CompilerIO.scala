package com.vanillasource.eliot.eliotc.processor

import cats.Monad
import cats.data.{Chain, EitherT, ReaderT, StateT}
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.CompilerError

object CompilerIO {
  private type EitherStage[T] = EitherT[IO, Chain[CompilerError], T]
  private type StateStage[T]  = StateT[EitherStage, Chain[CompilerError], T]
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
    getFact(key).flatMap {
      case Some(fact) => fact.pure[CompilerIO]
      case None       => abort[V]
    }

  /** Register an error.
    */
  def registerCompilerError(error: CompilerError): CompilerIO[Unit] =
    ReaderT.liftF[StateStage, CompilationProcess, Unit](
      StateT.modify[EitherStage, Chain[CompilerError]](errors => errors :+ error)
    )

  /** Returns true if there are no errors accumulated in the CompilerIO.
    */
  def isClear: CompilerIO[Boolean] = currentErrors.map(_.isEmpty)

  /** Returns the currently accumulated errors.
    */
  def currentErrors: CompilerIO[Chain[CompilerError]] =
    ReaderT.liftF[StateStage, CompilationProcess, Chain[CompilerError]](
      StateT.get[EitherStage, Chain[CompilerError]]
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

  /** Recovers from a potentially aborted computation by moving any errors from the Either's left side back into the
    * state. This is the opposite of abort - it ensures the computation continues (unaborted) while preserving all
    * errors. The computation runs in an isolated error context and all errors are merged back into the current context.
    *
    * @param computation
    *   The computation to run in isolation
    * @param default
    *   The value to return if the computation aborted
    * @return
    *   A computation that cannot abort (all errors are captured in state)
    */
  def recover[T](computation: CompilerIO[T])(default: T): CompilerIO[T] =
    for {
      process <- ReaderT.ask[StateStage, CompilationProcess]
      result  <- ReaderT.liftF[StateStage, CompilationProcess, Either[Chain[CompilerError], (Chain[CompilerError], T)]](
                   StateT.liftF(
                     EitherT.liftF(computation.run(process).run(Chain.empty).value)
                   )
                 )
      value   <- result match {
                   case Left(errors)           => errors.traverse_(registerCompilerError).as(default)
                   case Right((errors, value)) => errors.traverse_(registerCompilerError).as(value)
                 }
    } yield value

}
