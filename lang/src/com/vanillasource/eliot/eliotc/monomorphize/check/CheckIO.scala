package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.data.StateT
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO

object CheckIO {
  type CheckIO[A] = StateT[CompilerIO, CheckState, A]

  def get: CheckIO[CheckState]                           = StateT.get
  def pure[A](a: A): CheckIO[A]                          = StateT.pure(a)
  def modify(f: CheckState => CheckState): CheckIO[Unit] = StateT.modify(f)
  def inspect[A](f: CheckState => A): CheckIO[A]         = StateT.inspect(f)
  def liftF[A](fa: CompilerIO[A]): CheckIO[A]            = StateT.liftF(fa)
}
