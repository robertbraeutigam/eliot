package com.vanillasource.stm

import cats.effect.IO
import com.vanillasource.stm.STM.*

trait STMVar[A] {
  def get(): STM[A]

  def set(a: A): STM[Unit]

  def update(f: A => A): STM[Unit] = for {
    a <- get()
    _ <- set(f(a))
  } yield ()
}

object STMVar {
  extension (stmVar: STMVar[Int])(using stmRuntime: STMRuntime) {
    def incCommit(): IO[Unit] = stmVar.update(_ + 1).commit

    def decCommit(): IO[Unit] = stmVar.update(_ - 1).commit
  }
}
