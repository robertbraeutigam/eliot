package com.vanillasource.stm

trait STMVar[A] {
  def get(): STM[A]

  def set(a: A): STM[Unit]

  def update(f: A => A): STM[Unit] = for {
    a <- get()
    _ <- set(f(a))
  } yield ()
}
