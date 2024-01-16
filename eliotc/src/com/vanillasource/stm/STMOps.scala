package com.vanillasource.stm

sealed trait STMOps[A]

object STMOps {
  case class CreateSTMVar[T](initialValue: T)             extends STMOps[STMVar[T]]
  case class GetSTMVar[T](stmVar: STMVar[T])              extends STMOps[T]
  case class SetSTMVar[T](stmVar: STMVar[T], newValue: T) extends STMOps[Unit]
  case class Retry[A]()                                   extends STMOps[A]
  case class Error[A](t: Throwable)                       extends STMOps[A]
}
