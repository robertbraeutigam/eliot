package com.vanillasource.stm

trait STMVar[A] {
  def get(): STM[A]

  def set(a: A): STM[Unit]
}
